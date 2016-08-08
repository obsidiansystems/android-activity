{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, FlexibleContexts, NoMonoLocalBinds, RankNTypes, MultiParamTypeClasses, UndecidableInstances, ConstraintKinds, LambdaCase, AllowAmbiguousTypes, PartialTypeSignatures, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Focus.Backend.Listen where

import Focus.Account
import Focus.Api
import Focus.AppendMap (AppendMap (..))
import Focus.Backend.Schema.TH
import Focus.Schema
import Focus.Request
import Focus.Sign
import Focus.WebSocket

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (handle, try, SomeException, displayException)
import Control.Lens
import Control.Monad.State.Strict hiding (state)
import Control.Monad.Writer
import Data.Aeson
import Data.Align
import qualified Data.Map as Map
import Data.Pool
import Data.String
import Data.Text.Encoding
import Data.These
import Database.Groundhog
import Database.Groundhog.Core hiding (Update)
import qualified Database.Groundhog.Core as GH
import qualified Database.Groundhog.Expression as GH
import Database.Groundhog.Generic
import Database.Groundhog.Instances ()
import Database.Groundhog.Postgresql
import Database.PostgreSQL.Simple as PG
import Network.WebSockets as WS
import Network.WebSockets.Snap
import Snap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple.Notification as PG

type MonadListenDb m = (PersistBackend m, SqlDb (PhantomDb m))

data NotificationType = NotificationType_Insert | NotificationType_Update | NotificationType_Delete
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

makeJson ''NotificationType

withNotifications :: FromJSON a => Pool Postgresql -> (TChan a -> IO r) -> IO r
withNotifications db k = bracket (listenDB $ \f -> withResource db $ \(Postgresql conn) -> f conn) snd $ \(nm, _) -> k nm

updateChannel :: String
updateChannel = "updates"

insertAndNotify :: (PersistBackend m, DefaultKey a ~ AutoKey a, DefaultKeyId a, PersistEntity a, ToJSON (IdData a)) => a -> m (Id a)
insertAndNotify t = do
  tid <- liftM toId $ insert t
  notifyEntity NotificationType_Insert tid t
  return tid

insertByAllAndNotify :: (PersistBackend m, DefaultKey a ~ AutoKey a, DefaultKeyId a, PersistEntity a, ToJSON (IdData a)) => a -> m (Maybe (Id a))
insertByAllAndNotify t = do
  etid <- liftM (fmap toId) $ insertByAll t
  case etid of
    Left _ -> return Nothing
    Right tid -> notifyEntity NotificationType_Insert tid t >> return (Just tid)

insertByAllAndNotifyWithBody :: (PersistBackend m, DefaultKey a ~ AutoKey a, DefaultKeyId a, PersistEntity a, ToJSON a, ToJSON (IdData a)) => a -> m (Maybe (Id a))
insertByAllAndNotifyWithBody t = do
  etid <- liftM (fmap toId) $ insertByAll t
  case etid of
    Left _ -> return Nothing
    Right tid -> notifyEntityWithBody NotificationType_Insert tid t >> return (Just tid)

--TODO: remove type hole from signature; may need to modify groundhog to make that possible
updateAndNotify :: (ToJSON (IdData a), GH.Expression (PhantomDb m) (RestrictionHolder v c) (DefaultKey a), PersistEntity v, PersistEntity a, PersistBackend m, GH.Unifiable (AutoKeyField v c) (DefaultKey a), DefaultKeyId a, _) 
                => Id a 
                -> [GH.Update (PhantomDb m) (RestrictionHolder v c)]
                -> m ()
updateAndNotify tid dt = do
  update dt (AutoKeyField ==. fromId tid)
  notifyEntityId NotificationType_Update tid

notifyEntity :: (PersistBackend m, PersistEntity a, ToJSON (IdData a)) => NotificationType -> Id a -> a -> m ()
notifyEntity nt aid _ = notifyEntityId nt aid

notifyEntityWithBody' :: forall a b m. (PersistBackend m, PersistEntity a, ToJSON (IdData a), ToJSON b) => NotificationType -> Id a -> b -> m ()
notifyEntityWithBody' nt aid b = do
  let proxy = undefined :: proxy (PhantomDb m)
      cmd = "NOTIFY " <> updateChannel <> ", ?"
  _ <- executeRaw False cmd [PersistString $ T.unpack $ decodeUtf8 $ LBS.toStrict $ encode (nt, entityName $ entityDef proxy (undefined :: a), (aid, b))]
  return ()

notifyEntityWithBody :: (PersistBackend m, PersistEntity a, ToJSON (IdData a), ToJSON a) => NotificationType -> Id a -> a -> m ()
notifyEntityWithBody = notifyEntityWithBody'

notifyEntityId :: forall a m. (PersistBackend m, PersistEntity a, ToJSON (IdData a)) => NotificationType -> Id a -> m ()
notifyEntityId nt aid = do
  let proxy = undefined :: proxy (PhantomDb m)
  let cmd = "NOTIFY " <> updateChannel <> ", ?"
  _ <- executeRaw False cmd [PersistString $ T.unpack $ decodeUtf8 $ LBS.toStrict $ encode (nt, entityName $ entityDef proxy (undefined :: a), aid)]
  return ()

handleListen :: forall m m' rsp rq notification vs vp state.
                (MonadSnap m, ToJSON rsp, FromJSON rq,
                 FromJSON vs, ToJSON vp, Monoid vs)
             => (forall x. m' x -> IO x) -- runGroundhog
             -> TChan notification -- chan
             -> vs
             -> state
             -> (vs -> vs -> StateT state m' vp) -- getView
             -> (vs -> notification -> StateT state m' (Maybe vp)) -- getPatch
             -> (rq -> IO rsp) -- processRequest
             -> m () 
handleListen runGroundhog chan vs0 state0 getView getPatch processRequest = ifTop $ do
  changes <- liftIO . atomically $ dupTChan chan
  stateRef <- liftIO $ newMVar (vs0, state0)
  runWebSocketsSnap $ \pc -> do
    conn <- acceptRequest pc
    let send' = sendTextData conn . encodeR . Right . WebSocketData_Listen
    senderThread <- forkIO $ do
      vpInit <- modifyMVar stateRef $ \(vsInit, stateInit) -> do
        -- NB: newStateInit is forced to be strict below to prevent a buildup of thunks in the IORef
        (vpInit, !newStateInit) <- runGroundhog (runStateT (getView vsInit mempty) stateInit)
        -- atomicModifyIORef' stateRef $ \(vs, _) -> ((vs, newStateInit), ())
        return ((vsInit, newStateInit), vpInit)
      send' vpInit

      forever $ do
        change <- atomically $ readTChan changes
        vp <- modifyMVar stateRef $ \(vs, state) -> do
          -- NB: newState is forced to be strict below to prevent a buildup of thunks in the IORef
          (vp, !newState) <- runGroundhog $ runStateT (getPatch vs change) state
          return ((vs, newState), vp)
        mapM_ send' vp
    let handleConnectionException = handle $ \e -> case e of
          ConnectionClosed -> return ()
          _ -> print e
    handleConnectionException $ forever $ do
      dm <- receiveDataMessage conn
      let (wrapper, r) = case dm of
            WS.Text r' -> (WS.Text, r')
            WS.Binary r' -> (WS.Text, r') --TODO
          sender rid act = do
            er <- try act
            sendDataMessage conn . wrapper . encodeR . Right . WebSocketData_Api rid $ case er of
              Left (se :: SomeException) -> Left (displayException se)
              Right rsp -> Right rsp
      case eitherDecode' r of
        Left s -> sendDataMessage conn . wrapper . encodeR $ Left (mconcat ["error: ", s, "\n", "received: ", show r])
        Right (WebSocketData_Api rid rq) -> sender rid $ processRequest rq
        Right (WebSocketData_Listen vs) -> do
          vp <- modifyMVar stateRef $ \(vsOld, state) -> do
            -- NB: newState is forced to be strict below to prevent a buildup of thunks in the IORef
            (vp, !newState) <- runGroundhog $ runStateT (getView vs vsOld) state
            return ((vs, newState), vp)
          send' vp
    killThread senderThread
 where
   encodeR :: Either String (WebSocketData vp (Either String rsp)) -> LBS.ByteString
   encodeR = encode

getViewsForTokens :: (Monad m, Align (AppendMap token), Monoid vs) => (token -> vs -> vs -> m vp) -> AppendMap token vs -> AppendMap token vs -> m (AppendMap token vp)
getViewsForTokens getView vs vsOld = do
  viewMap <- iforM (align vs vsOld) $ \token -> \case
    -- Logout
    That _ -> return Nothing
    -- Login
    This vsNew -> fmap Just $ getView token vsNew mempty
    -- Update for a logged in user
    These vsNew vsOld' -> fmap Just $ getView token vsNew vsOld'
  return $ (AppendMap . Map.mapMaybe id . _unAppendMap) viewMap

getPatchesForTokens :: Monad m => (token -> vs -> n -> m (Maybe vp)) -> AppendMap token vs -> n -> m (Maybe (AppendMap token vp))
getPatchesForTokens getPatch vs notification = do
  m <- iforM vs $ \token vs' -> getPatch token vs' notification
  let m' = (AppendMap . Map.mapMaybe id . _unAppendMap) m
  return $ if Map.null (_unAppendMap m') then Nothing else Just m'

listenDB :: FromJSON a => (forall x. (PG.Connection -> IO x) -> IO x) -> IO (TChan a, IO ())
listenDB withConn' = do
  nChan <- newBroadcastTChanIO
  daemonThread <- forkIO $ withConn' $ \conn -> do
    let cmd = fromString $ "LISTEN " <> updateChannel
    execute_ conn cmd
    forever $ do
      PG.Notification _ channel message <- PG.getNotification conn
      case channel of
        _ | channel == encodeUtf8 (T.pack updateChannel) -> do
          case decodeValue' $ LBS.fromStrict message of
            Just a -> atomically $ writeTChan nChan a
            _ -> putStrLn $ "listenDB: Could not parse message on updates channel: " <> show message
        _ -> putStrLn $ "listenDB: Received a message on unexpected channel: " <> show channel 
  return (nChan, killThread daemonThread)

handleRequests :: forall f m pub priv. (Monad m)
                => (forall x. ToJSON x => f x -> m Value) -- Runs request and turns response into JSON
                -> (forall x. pub x -> f x) -- Public request handler 
                -> (forall x. Signed AuthToken -> priv x -> f x) -- Private request handler
                -> SomeRequest (ApiRequest pub priv) -- Api Request
                -> m Value -- JSON response
handleRequests runRequest fpub fpriv request = case request of
  SomeRequest req -> do
    rsp <- case req of
      ApiRequest_Public r -> runRequest (fpub r)
      ApiRequest_Private token r -> runRequest (fpriv token r)
    return rsp

