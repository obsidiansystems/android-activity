{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, FlexibleContexts, NoMonoLocalBinds, RankNTypes, MultiParamTypeClasses, UndecidableInstances, ConstraintKinds #-}
module Focus.Backend.Listen where

import Focus.Backend.Schema.TH
import Focus.Schema
import Focus.Request
import Snap
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.Instances ()
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow
import Control.Monad.Writer
import Network.WebSockets
import Network.WebSockets.Snap
import Control.Exception (handle)
import Control.Concurrent
import Control.Concurrent.STM
import Database.PostgreSQL.Simple as PG
import Data.Pool
import qualified Database.PostgreSQL.Simple.Notification as PG
import Database.Groundhog.Postgresql
import Data.String
import Data.ByteString (ByteString)

type MonadListenDb m = (PersistBackend m, SqlDb (PhantomDb m))

newtype PerClientListener a n = PerClientListener (forall m. MonadListenDb m => a -> m [n])

data TableListener a n
   = TableListener { tableListenerGetInitial :: forall m. MonadListenDb m => a -> m [n]
                   , tableListenerGetUpdate :: forall m. MonadListenDb m => LBS.ByteString -> m (PerClientListener a n)
                   }

type Listeners a n = Map ByteString (TableListener a n)

withNotifications :: Pool Postgresql -> Listeners a n -> (TChan (PerClientListener a n) -> IO r) -> IO r
withNotifications db notifications k = bracket (listenDB notifications $ \f -> withResource db $ \(Postgresql conn) -> f conn) snd $ \(nm, _) -> k nm

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
  let cmd = "NOTIFY " <> show (entityName $ entityDef proxy (undefined :: a)) <> ", ?"
  _ <- executeRaw False cmd [PersistString $ T.unpack $ decodeUtf8 $ LBS.toStrict $ encode aid]
  return ()

tableListenerWithAutoKey :: forall a n b. (PersistEntity a, DefaultKeyId a, DefaultKey a ~ AutoKey a, DefaultKey a ~ Key a BackendSpecific, PrimitivePersistField (DefaultKey a), FromJSON (IdData a)) => ((Id a, a) -> n) -> TableListener b n
tableListenerWithAutoKey n = TableListener
      { tableListenerGetInitial = \_ -> liftM (map $ n . first toId) selectAll
      , tableListenerGetUpdate = \xidStr -> do
        Just (xid :: Id a) <- return $ decodeValue' xidStr
        mx :: Maybe a <- get $ fromId xid
        case mx of
          Nothing -> return $ PerClientListener $ const $ return []
          Just x -> return $ PerClientListener $ const $ return [n (xid, x)]
      }

handleListen :: (MonadSnap m, MonadIO m, MonadListenDb m', ToJSON n) => a -> Listeners a n -> TChan (PerClientListener a n) -> (forall x. m' x -> IO x) -> (DataMessage -> IO ()) -> m ()
handleListen a listeners l runGroundhog onReceive = ifTop $ do
  changes <- liftIO $ atomically $ dupTChan l
  startingValues <- liftIO $ runGroundhog $ do
    startingValues <- mapM (flip tableListenerGetInitial a) $ Map.elems listeners
    return $ concat startingValues
  runWebSocketsSnap $ \pc -> do
    conn <- acceptRequest pc
    senderThread <- forkIO $ do
      let send' = sendTextData conn . encode
      send' startingValues
      forever $ do
        (PerClientListener change) <- atomically $ readTChan changes
        change' <- runGroundhog $ change a
        send' change'
    let handleConnectionException = handle $ \e -> case e of
          ConnectionClosed -> return ()
          _ -> print e
    handleConnectionException $ forever $ receiveDataMessage conn >>= onReceive
    killThread senderThread

listenDB :: forall a n. Listeners a n -> (forall x. (PG.Connection -> IO x) -> IO x) -> IO (TChan (PerClientListener a n), IO ())
listenDB listeners withConn' = do
  nChan <- newBroadcastTChanIO
  daemonThread <- forkIO $ withConn' $ \conn -> do
    forM_ (Map.keys listeners) $ \k -> do
      let cmd = fromString $ "LISTEN " <> show k
      execute_ conn cmd
    forever $ do
      PG.Notification _ channel message <- PG.getNotification conn
      case Map.lookup channel listeners of
        Nothing -> putStrLn $ "listenDB: received message from unknown channel: " <> show channel
        Just l -> do
          translation <- withConn' $ (runDbConn $ tableListenerGetUpdate l $ LBS.fromStrict message) . Postgresql
          atomically $ writeTChan nChan translation
  return (nChan, killThread daemonThread)

-- The 'account' type is typically Maybe (Id Account), while 'notification' is the app's Notification API type.
tableListener :: forall a b account notification. (FromJSON (IdData a), FromJSON a)
              => ((Id a, b) -> notification)
              -> (forall m. MonadListenDb m => account -> Maybe (Id a) -> m [(Id a, b)])
              -> TableListener account notification
tableListener n f = TableListener
    { tableListenerGetInitial = \aid -> liftM (map n) (f aid Nothing)
    , tableListenerGetUpdate = \xidStr -> do
        Just (xid :: Id a) <- return $ decodeValue' xidStr
        return $ PerClientListener $ \aid -> liftM (map n) $ f aid (Just xid)
    }

