{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, FlexibleContexts, NoMonoLocalBinds, RankNTypes, MultiParamTypeClasses, UndecidableInstances, ConstraintKinds #-}
module Focus.Backend.Listen where

import Focus.Backend.Schema.TH
import Focus.Schema

import Focus.Request
import Snap hiding (get)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Control.Exception (evaluate)
import Data.Int
import Control.Lens
import Data.Pool
import Database.Groundhog
import Database.Groundhog.Core hiding (Proxy (..))
import Database.Groundhog.Expression
import Database.Groundhog.Generic
import Database.Groundhog.Instances
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time
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
import qualified Database.PostgreSQL.Simple.Notification as PG
import Database.Groundhog.Postgresql
import Data.String
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Maybe

type MonadListenDb m = (PersistBackend m, SqlDb (PhantomDb m))

newtype PerClientListener a n = PerClientListener (forall m. MonadListenDb m => a -> m [n])

data TableListener a n
   = TableListener { tableListenerGetInitial :: forall m. MonadListenDb m => a -> m [n]
                   , tableListenerGetUpdate :: forall m. MonadListenDb m => LBS.ByteString -> m (PerClientListener a n)
                   }

type Listeners a n = Map ByteString (TableListener a n)

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

handleListen :: (MonadSnap m, MonadIO m, MonadListenDb m', ToJSON n) => a -> Listeners a n -> TChan (PerClientListener a n) -> (forall x. m' x -> IO x) -> m ()
handleListen a listeners l runGroundhog = ifTop $ do
  changes <- liftIO $ atomically $ dupTChan l
  startingValues <- liftIO $ runGroundhog $ do
    startingValues <- mapM (flip tableListenerGetInitial a) $ Map.elems listeners
    return $ concat startingValues
  runWebSocketsSnap $ \pc -> do
    conn <- acceptRequest pc
    senderThread <- forkIO $ do
      let send = sendTextData conn . encode
      send startingValues
      forever $ do
        (PerClientListener change) <- atomically $ readTChan changes
        change' <- runGroundhog $ change a
        send change'
    let handleConnectionException = handle $ \e -> case e of
          ConnectionClosed -> return ()
          _ -> print e
    handleConnectionException $ forever $ receiveDataMessage conn
    killThread senderThread

listenDB :: forall a n. Listeners a n -> (forall a. (PG.Connection -> IO a) -> IO a) -> IO (TChan (PerClientListener a n), IO ())
listenDB listeners withConn = do
  nChan <- newBroadcastTChanIO
  daemonThread <- forkIO $ withConn $ \conn -> do
    forM_ (Map.keys listeners) $ \k -> do
      let cmd = fromString $ "LISTEN " <> show k
      execute_ conn cmd
    forever $ do
      PG.Notification _ channel message <- PG.getNotification conn
      case Map.lookup channel listeners of
        Nothing -> putStrLn $ "listenDB: received message from unknown channel: " <> show channel
        Just l -> do
          translation <- withConn $ (runDbConn $ tableListenerGetUpdate l $ LBS.fromStrict message) . Postgresql
          atomically $ writeTChan nChan translation
  return (nChan, killThread daemonThread)
