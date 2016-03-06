{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, FlexibleContexts, NoMonoLocalBinds, RankNTypes, MultiParamTypeClasses, UndecidableInstances, ConstraintKinds #-}
module Focus.Backend.Listen where

import Focus.Backend.Schema.TH
import Focus.Schema
import Focus.Request
import Focus.WebSocket

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (handle, try, SomeException, displayException)
import Control.Monad.Writer
import Data.Aeson
import Data.IORef
import Data.Pool
import Data.String
import Data.Text.Encoding
import Database.Groundhog
import Database.Groundhog.Core
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

withNotifications :: FromJSON a => Pool Postgresql -> (TChan a -> IO r) -> IO r
withNotifications db k = bracket (listenDB $ \f -> withResource db $ \(Postgresql conn) -> f conn) snd $ \(nm, _) -> k nm

updateChannel :: String
updateChannel = "updates"

insertAndNotify :: (PersistBackend m, DefaultKey a ~ AutoKey a, DefaultKeyId a, PersistEntity a, ToJSON a, ToJSON (IdData a)) => a -> m (Id a)
insertAndNotify t = do
  tid <- liftM toId $ insert t
  notifyEntity tid t
  return tid

notifyEntity :: (PersistBackend m, PersistEntity a, ToJSON (IdData a), ToJSON a) => Id a -> a -> m ()
notifyEntity aid _ = notifyEntityId aid

notifyEntityId :: forall a m. (PersistBackend m, PersistEntity a, ToJSON (IdData a), ToJSON a) => Id a -> m ()
notifyEntityId aid = do
  let proxy = undefined :: proxy (PhantomDb m)
  let cmd = "NOTIFY " <>  updateChannel <> ", ?"
  _ <- executeRaw False cmd [PersistString $ T.unpack $ decodeUtf8 $ LBS.toStrict $ encode (entityName $ entityDef proxy (undefined :: a), aid)]
  return ()

handleListen :: forall m m' rsp rq notification vs v.
                (MonadSnap m, MonadIO m, MonadListenDb m', ToJSON rsp, FromJSON rq, FromJSON notification, FromJSON vs, ToJSON v)
             => (forall x. m' x -> IO x)
             -> TChan notification
             -> vs
             -> (vs -> m' v)
             -> (rq -> IO rsp)
             -> m ()
handleListen runGroundhog chan vs0 updateView processRequest = ifTop $ do
  changes <- liftIO $ atomically $ dupTChan chan
  vsRef <- liftIO $ newIORef vs0
  let updateView' = do
        vs <- readIORef vsRef
        runGroundhog $ updateView vs
  runWebSocketsSnap $ \pc -> do
    conn <- acceptRequest pc
    let send' = sendTextData conn . encodeR . WebSocketData_Listen
    senderThread <- forkIO $ do
      send' =<< updateView'
      forever $ do
        _ <- atomically $ readTChan changes
        send' =<< updateView'
    let handleConnectionException = handle $ \e -> case e of
          ConnectionClosed -> return ()
          _ -> print e
    handleConnectionException $ forever $ do
      dm <- receiveDataMessage conn
      let (wrapper, r) = case dm of
            WS.Text r' -> (WS.Text, r')
            WS.Binary r' -> (WS.Binary, r')
          sender rid act = do
            er <- try act
            sendDataMessage conn . wrapper . encodeR . WebSocketData_Api rid $ case er of
              Left (se :: SomeException) -> Left (displayException se)
              Right rsp -> Right rsp
      case eitherDecode' r of
        Left _ -> return ()
        Right (WebSocketData_Api rid rq) -> sender rid $ processRequest rq
        Right (WebSocketData_Listen vs) -> do
          atomicModifyIORef vsRef (const (vs, ()))
          send' =<< updateView'
    killThread senderThread
 where encodeR :: WebSocketData v (Either String rsp) -> LBS.ByteString
       encodeR = encode

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
