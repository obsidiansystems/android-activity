{-# LANGUAGE Rank2Types, ScopedTypeVariables, GeneralizedNewtypeDeriving, LambdaCase, TemplateHaskell, KindSignatures, OverloadedStrings, FlexibleInstances, FlexibleContexts, ConstraintKinds #-}
module Focus.Client ( module Focus.Client
                    , module Focus.Client.Types
                    ) where

import Control.Concurrent (forkFinally, killThread, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import Debug.Trace.LocationTH

import Network.Socket
import Network.WebSockets hiding (ClientApp, Request)

import Focus.Account
import Focus.Api
import Focus.AppendMap
import Focus.Request
import Focus.Sign
import Focus.WebSocket

import Focus.Client.Types

requestEnv :: forall pub priv select view patch. (Request pub, Request priv) => ClientEnv select view patch -> RequestEnv pub priv select view
requestEnv c = RequestEnv
  { _requestEnv_sendRequest = \req ->
      let sr = _clientEnv_nextRequestId c
          pending = _clientEnv_pendingRequests c
          setup = atomically $ do
            s <- readTVar sr
            modifyTVar' sr (+1)
            em <- newTBQueue 1
            modifyTVar' pending $ Map.insertWith ($failure "duplicate request id") s em
            return (s, em)
          teardown s = atomically $ modifyTVar' pending $ Map.delete s
          run :: (RequestId, TBQueue (Either String Value)) -> IO (Async (Either String Value))
          run (s,em) =
            let payload :: WebSocketData (Maybe (Signed AuthToken)) Value (SomeRequest (ApiRequest pub priv))
                payload = WebSocketData_Api (toJSON s) (SomeRequest req)
            in sendTextData conn (encode payload) >> async (atomically (readTBQueue em) `finally` teardown s)
      in setup >>= run
  , _requestEnv_registerInterest = \token sel -> do
      let si = _clientEnv_nextInterestId c
          interests = _clientEnv_interests c
      atomically $ do
        s <- readTVar si
        modifyTVar' si (+1)
        modifyTVar' interests $ Map.insertWith ($failure "duplicate interest id") s (token, sel)
        return $ (s, modifyTVar' interests $ Map.delete s)
  , _requestEnv_sendInterestSet = do
      is <- Map.foldr (<>) mempty . fmap (AppendMap . uncurry Map.singleton) <$> readTVarIO (_clientEnv_interests c)
      let payload :: WebSocketData (Signed AuthToken) select Value
          payload = WebSocketData_Listen is
      sendTextData conn $ encode payload
  , _requestEnv_listen = \t s l -> do
      let onChange = _clientEnv_notifyViewChange c
      viewChange <- atomically $ dupTChan onChange
      async $ do
        v <- readTVarIO (_clientEnv_viewMap c)
        case l . _clientEnv_cropView c s =<< Map.lookup t v of
          Just r -> return r
          Nothing -> fix $ \k -> do
            mr <- atomically $ do
              readTChan viewChange
              mv <- fmap (_clientEnv_cropView c s) . Map.lookup t <$> readTVar (_clientEnv_viewMap c)
              return $ l =<< mv
            case mr of
              Nothing -> k
              Just r -> return r
  }
 where
  conn = _clientEnv_connection c

--TODO: Error reporting when the client is expecting the wrong patch type and the decode always fails
listener :: FromJSON patch => ClientEnv select view patch -> IO ()
listener env = handleConnectionException $ forever $ runMaybeT $ do
  raw <- lift $ receiveData (_clientEnv_connection env)
  (mma :: Either String (WebSocketData (Signed AuthToken) patch (Either String Value))) <- MaybeT . return $ decodeValue' raw
  ma <- MaybeT . return . either (\_ -> Nothing) Just $ mma
  liftIO $ atomically $ runMaybeT $ case ma of
    WebSocketData_Listen (AppendMap pmap) -> do
      let applyPatch = _clientEnv_patchView env
      lift $ modifyTVar' (_clientEnv_viewMap env) $ Map.mergeWithKey
        (\_ x y -> Just (applyPatch x y))
        (fmap (\p -> applyPatch p (_clientEnv_emptyView env)))
        (const Map.empty)
        pmap
      lift (writeTChan (_clientEnv_notifyViewChange env) ())
      return ()
    WebSocketData_Api rid' eea -> do
      rid <- parseToMaybeT $ fromJSON rid'
      mv <- MaybeT (Map.lookup rid <$> readTVar (_clientEnv_pendingRequests env))
      lift (writeTBQueue mv eea)
      return ()
 where
  parseToMaybeT r = MaybeT . return $ case r of
   Error _ -> Nothing
   Success r' -> Just r'
  handleConnectionException = handle $ \e -> case e of
    ConnectionClosed -> return ()
    _ -> print e

request :: (MonadRequest pub priv select view m, ToJSON rsp, FromJSON rsp) => ApiRequest pub priv rsp -> m (RequestResult rsp)
request req = do
  reqP <- asks _requestEnv_sendRequest >>= \sendReq -> liftIO (sendReq req)
  mt <- gets _requestState_timeout
  er <- liftIO $ race (timeout mt >> return ()) (processResult <$> waitCatch reqP)
  return $ case er of
    Left _ -> RequestResult_Timeout (fromJust mt)
    Right r -> r
 where
  timeout = \case
    Nothing -> forever $ threadDelay 10000000
    Just t -> threadDelay t
  processResult = \case
    Left e -> RequestResult_Failure (show e)
    Right (Left e) -> RequestResult_Failure e
    Right (Right r) -> case fromJSON r of
      Error e -> RequestResult_DecodeError r e
      Success r' -> RequestResult_Success r'

private :: (MonadRequest pub priv select view m, ToJSON rsp, FromJSON rsp) => priv rsp -> m (RequestResult rsp)
private req = do
  mtoken <- gets _requestState_token
  case mtoken of
    Nothing -> return RequestResult_RequiresAuthorization
    Just token -> request (ApiRequest_Private token req)

public :: (MonadRequest pub priv select view m, ToJSON rsp, FromJSON rsp) => pub rsp -> m (RequestResult rsp)
public req = request (ApiRequest_Public req)

listen :: (MonadRequest pub priv select view m, Monoid select) => (view -> Maybe a) -> m (ListenResult a)
listen l = do
  mtoken <- gets _requestState_token
  case mtoken of
    Nothing -> return ListenResult_RequiresAuthorization
    Just t -> do
      mListen <- asks _requestEnv_listen
      mTimeout <- gets _requestState_timeout
      i <- Map.foldr (\sel acc -> fst sel <> acc) mempty <$> gets _requestState_interests
      er <- liftIO $ race (timeout mTimeout >> return ()) (fmap processListen . waitCatch =<< mListen t i l)
      return $ case er of
        Left _ -> ListenResult_Timeout (fromJust mTimeout)
        Right r -> r
 where
   timeout = \case
     Nothing -> forever $ threadDelay 10000000
     Just t -> threadDelay t
   processListen = \case
     Left e -> ListenResult_Failure (show e)
     Right r -> ListenResult_Success r

setToken :: (MonadRequest pub priv select view m) => Maybe (Signed AuthToken) -> m (Maybe (Signed AuthToken))
setToken t = requestState_token <<.= t

withToken :: (MonadRequest pub priv select view m) => Maybe (Signed AuthToken) -> m a -> m a
withToken mt a =
  bracket setup teardown run
 where
  setup = setToken mt
  teardown t' = setToken t'
  run _ = a

tellInterest :: (MonadRequest pub priv select view m) => select -> m (Maybe ())
tellInterest s = do
  mt <- gets _requestState_token
  case mt of
    Nothing -> return Nothing
    Just t -> do
      registerInterest <- asks _requestEnv_registerInterest
      sendInterestSet <- asks _requestEnv_sendInterestSet
      (sid, unregister) <- liftIO $ registerInterest t s
      requestState_interests %= Map.insert sid (s, unregister)
      liftIO sendInterestSet
      return (Just ())

withInterest :: (MonadRequest pub priv select view m) => select -> m a -> m (Maybe a)
withInterest s a = do
  mt <- gets _requestState_token
  case mt of
    Nothing -> return Nothing
    Just t ->
      let setup = do
            registerInterest <- asks _requestEnv_registerInterest
            sendInterestSet <- asks _requestEnv_sendInterestSet
            (sid, unregister) <- liftIO $ registerInterest t s
            requestState_interests %= Map.insert sid (s, unregister)
            liftIO sendInterestSet
            return (sid, unregister)
          teardown (sid, unregister) = do
            liftIO $ atomically unregister
            requestState_interests %= Map.delete sid
      in Just <$> bracket setup teardown (\_ -> a)

runClientApp :: (Request pub, Request priv, FromJSON patch, Monoid select, ToJSON select) => RequestM pub priv select view a -> ClientConfig select view patch -> IO a
runClientApp m cfg = withSocketsDo $ do
  result <- newEmptyTMVarIO
  handleConnectionException $ runClient (_clientConfig_host cfg)
                                        (_clientConfig_port cfg)
                                        (_clientConfig_path cfg) $ \conn -> do
    cenv <- atomically $ do
      nextReq <- newTVar 1
      pending <- newTVar Map.empty
      views' <- newTVar Map.empty
      notify <- newBroadcastTChan
      nextInterest <- newTVar 1
      interests <- newTVar Map.empty
      return $ ClientEnv { _clientEnv_connection = conn
                         , _clientEnv_nextRequestId = nextReq
                         , _clientEnv_pendingRequests = pending
                         , _clientEnv_emptyView = _clientConfig_emptyView cfg
                         , _clientEnv_viewMap = views'
                         , _clientEnv_patchView = _clientConfig_patchView cfg
                         , _clientEnv_cropView = _clientConfig_cropView cfg
                         , _clientEnv_notifyViewChange = notify
                         , _clientEnv_nextInterestId = nextInterest
                         , _clientEnv_interests = interests
                         }
    let renv = requestEnv cenv
    listenDone <- newEmptyTMVarIO
    bracket
      (forkFinally (listener cenv) (\_ -> atomically (putTMVar listenDone ())))
      (\lid -> killThread lid >> atomically (takeTMVar listenDone) >> handleConnectionException (sendClose conn ("Goodbye" :: Text) >> forever (receiveDataMessage conn))) $
      \_ -> do
        er <- try $ runRWST (_runRequestM m) renv (RequestState Nothing (_clientConfig_timeout cfg) Map.empty)
        case er of
          Left (e :: SomeException) -> atomically (putTMVar result (Left e))
          Right (a,s,_) -> atomically (forM_ (_requestState_interests s) snd >> putTMVar result (Right a)) >> (_requestEnv_sendInterestSet renv)
  er <- atomically (takeTMVar result)
  case er of
    Left e -> throwM e
    Right r -> return r
 where
  handleConnectionException = handle $ \e -> case e of
    ConnectionClosed -> return ()
    _ -> print e
