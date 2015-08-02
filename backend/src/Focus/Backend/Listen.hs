{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, FlexibleContexts, NoMonoLocalBinds, RankNTypes, MultiParamTypeClasses, UndecidableInstances #-}
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

data TableListener n
   = TableListener { tableListenerGetInitial :: forall m. PersistBackend m => m [n]
                   , tableListenerGetUpdate :: forall m. PersistBackend m => LBS.ByteString -> m [n]
                   }

type Listeners n = Map ByteString (TableListener n)

notifyEntity :: (PersistBackend m, PersistEntity a, ToJSON (IdData a), ToJSON a) => Id a -> a -> m ()
notifyEntity aid _ = notifyEntityId aid

notifyEntityId :: forall a m. (PersistBackend m, PersistEntity a, ToJSON (IdData a), ToJSON a) => Id a -> m ()
notifyEntityId aid = do
  let proxy = undefined :: proxy (PhantomDb m)
  let cmd = "NOTIFY " <> show (entityName $ entityDef proxy (undefined :: a)) <> ", ?"
  _ <- executeRaw False cmd [PersistString $ T.unpack $ decodeUtf8 $ LBS.toStrict $ encode aid]
  return ()

tableListenerWithAutoKey :: (PersistEntity a, DefaultKeyId a, DefaultKey a ~ AutoKey a, DefaultKey a ~ Key a BackendSpecific, PrimitivePersistField (DefaultKey a), FromJSON (IdData a)) => ((Id a, a) -> n) -> TableListener n
tableListenerWithAutoKey n = TableListener
      { tableListenerGetInitial = liftM (map $ n . first toId) selectAll
      , tableListenerGetUpdate = \xidStr -> do
        Just xid <- return $ decodeValue' xidStr
        mx <- get $ fromId xid
        case mx of
          Nothing -> return []
          Just x -> return [n (xid, x)]
      }

handleListen :: (MonadSnap m, PersistBackend m', ToJSON n) => Listeners n -> TChan n -> (forall x. m' x -> m x) -> m ()
handleListen listeners l runGroundhog = ifTop $ do
  changes <- liftIO $ atomically $ dupTChan l
  startingValues <- runGroundhog $ do
    startingValues <- mapM tableListenerGetInitial $ Map.elems listeners
    return $ concat startingValues
  runWebSocketsSnap $ \pc -> do
    conn <- acceptRequest pc
    senderThread <- forkIO $ do
      let send = sendTextData conn . encode
      send startingValues
      forever $ do
        change <- atomically $ readTChan changes
        send [change]
    let handleConnectionException = handle $ \e -> case e of
          ConnectionClosed -> return ()
          _ -> print e
    handleConnectionException $ forever $ receiveDataMessage conn
    killThread senderThread

listenDB :: forall n. Listeners n -> (forall a. (PG.Connection -> IO a) -> IO a) -> IO (TChan n, IO ())
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
          mapM_ (atomically . writeTChan nChan) translation
  return (nChan, killThread daemonThread)
