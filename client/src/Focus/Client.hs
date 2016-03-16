{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, LambdaCase, TemplateHaskell, KindSignatures, OverloadedStrings  #-}
module Focus.Client where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text as T
import Debug.Trace.LocationTH
import System.Timeout

import Network.HTTP.Types.URI
import Network.Socket
import Network.WebSockets hiding (ClientApp, Request)

import Focus.Account
import Focus.Api
import Focus.Request
import Focus.Sign
import Focus.WebSocket

newtype RequestId = RequestId { _unTestId :: Int64 }
  deriving (Eq, Ord, Num, Show, Read, ToJSON, FromJSON)

data ClientEnv = ClientEnv { _clientEnv_token :: Maybe (Signed AuthToken)
                           , _clientEnv_websocket :: Connection
                           }

data ClientState = ClientState { _clientState_nextId :: MVar RequestId
                               , _clientState_pending :: MVar (Map RequestId (MVar (Either String Value)))
                               }

newtype ClientApp (pub :: * -> *) (priv :: * -> *) a = ClientApp { _runClientApp :: ReaderT ClientEnv (StateT ClientState IO) a }
  deriving (Functor, Applicative, Monad, MonadReader ClientEnv, MonadIO)

runClientApp :: ClientApp pub priv a -> Maybe (Signed AuthToken) -> IO a
runClientApp (ClientApp m) auth = withSocketsDo $ runClient "localhost" 8000 url $ \conn -> do
  nextId <- newMVar 1
  pending <- newMVar Map.empty
  lid <- forkIO (listener conn pending)
  a <- evalStateT (runReaderT m (ClientEnv auth conn)) (ClientState nextId pending)
  --TODO: Is this ok? What about pending messages in the pipe?
  killThread lid
  sendClose conn ("Bye! ;) ;) ;)" :: Text)
  return a
 where url = "/listen?token=" <> (T.unpack . decodeUtf8 . urlEncode True . LBS.toStrict . encode $ auth)

requestWithTimeout :: forall rsp pub priv. (ToJSON rsp, FromJSON rsp, Request pub, Request priv) => Maybe Int64 -> ApiRequest pub priv rsp -> ClientApp pub priv (Maybe (Either String rsp))
requestWithTimeout mtime req = ClientApp $ do
  conn <- asks _clientEnv_websocket
  (ClientState next pending) <- get
  liftIO $ do
    rid <- modifyMVar next $ \s -> return (s+1, s)
    em <- newEmptyMVar
    modifyMVar_ pending $ return . Map.insertWith ($failure "duplicate request id") rid em
    let payload :: WebSocketData Value (SomeRequest (ApiRequest pub priv))
        payload = WebSocketData_Api (toJSON rid) (SomeRequest req)
    sendTextData conn $ encode payload
    let decodeOrBust r = case fromJSON r of
          Error _ -> $failure $ "response failed to parse: " <> LBSC8.unpack (encode r)
          Success r' -> r'
    mrsp <- fmap decodeOrBust <$> readMVar em & case mtime of
      Nothing -> fmap Just
      Just t -> timeout (fromIntegral t)
    modifyMVar_ pending $ return . Map.delete rid
    return mrsp

privateWithTimeout :: (ToJSON rsp, FromJSON rsp, Request pub, Request priv) => Maybe Int64 -> priv rsp -> ClientApp pub priv (Maybe (Either String rsp))
privateWithTimeout mt req = do
  ma <- asks _clientEnv_token
  case ma of
    Nothing -> $failure $ "attempted to make private request without authentication: " <> LBSC8.unpack (encode (requestToJSON req))
    Just auth -> requestWithTimeout mt (ApiRequest_Private auth req)

publicWithTimeout :: (ToJSON rsp, FromJSON rsp, Request pub, Request priv) => Maybe Int64 -> pub rsp -> ClientApp pub priv (Maybe (Either String rsp))
publicWithTimeout mt req = requestWithTimeout mt (ApiRequest_Public req)

listener :: Connection -> MVar (Map RequestId (MVar (Either String Value))) -> IO ()
listener conn pending = forever $ do
  raw <- receiveData conn
  --TODO: This should be in the maybe monad or something; kill the staircase
  case decodeValue' raw of
    Nothing -> return () --TODO: error handling
    Just (mma :: Either String (WebSocketData Value (Either String Value))) -> case mma of
      Left _ -> return () --TODO: error handling
      Right ma -> case ma of
        WebSocketData_Auth _ -> return () --TODO: auth handling
        WebSocketData_Listen _ -> return () --TODO: listener handling
        WebSocketData_Api rid' eea -> case fromJSON rid' of
          Error _ -> return () --TODO: error handling
          Success rid -> do
            tryReadMVar pending >>= \case
              Nothing -> return () --TODO: error handling
              Just mp -> case Map.lookup rid mp of
                Nothing -> return () -- Resopnse is stale
                Just mv -> tryPutMVar mv eea >>= \case
                  False -> return () --TODO: error handling
                  True -> return ()
