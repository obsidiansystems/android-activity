{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, FlexibleContexts, NoMonoLocalBinds, RankNTypes, MultiParamTypeClasses, UndecidableInstances #-}
module Backend.Listen where

import Backend.Schema.TH
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
notifyEntity aid a = do
  _ <- executeRaw False ("NOTIFY " <> show (entityName $ entityDef a) <> ", ?") [PersistString $ T.unpack $ decodeUtf8 $ LBS.toStrict $ encode (aid, a)]
  return ()

notifyEntityId :: (PersistBackend m, PersistEntity a, ToJSON (IdData a), ToJSON a, Key a BackendSpecific ~ DefaultKey a, PrimitivePersistField (Key a BackendSpecific), DefaultKeyId a) => Id a -> m ()
notifyEntityId aid = do
  a <- get $ fromId aid
  maybe (return ()) (notifyEntity aid) a
  return ()

tableListenerWithAutoKey :: (PersistEntity a, DefaultKeyId a, DefaultKey a ~ AutoKey a, DefaultKey a ~ Key a BackendSpecific, PrimitivePersistField (DefaultKey a), FromJSON (IdData a)) => ((Id a, a) -> n) -> TableListener n
tableListenerWithAutoKey n = TableListener
      { tableListenerGetInitial = liftM (map $ n . first toId) selectAll
      , tableListenerGetUpdate = \xidStr -> do
        mVals <- forM (maybeToList $ decodeValue' xidStr) $ \xid -> do
          mx <- get $ fromId xid
          return $ fmap ((,) xid) mx
        return $ map n $ catMaybes mVals
      }

handleListen :: (PersistBackend m, ToJSON n) => Listeners n -> TChan n -> (forall x. m x -> Handler b a x) -> Handler b a ()
handleListen listeners l runGroundhog = ifTop $ do
  changes <- liftIO $ atomically $ dupTChan l
  startingValues <- runGroundhog $ do
    startingValues <- mapM tableListenerGetInitial $ Map.elems listeners
    return $ concat startingValues
  runWebSocketsSnap $ \pc -> do
    conn <- acceptRequest pc
    senderThread <- forkIO $ do
      let send = sendBinaryData conn . encode
      mapM send startingValues
      forever $ do
        change <- atomically $ readTChan changes
        send change
    handle (\ConnectionClosed -> return ()) $ forever $ receiveDataMessage conn

listenDB :: forall n. Listeners n -> (forall a. (PG.Connection -> IO a) -> IO a) -> IO (TChan n, IO ())
listenDB listeners withConn = do
  nChan <- newBroadcastTChanIO
  daemonThread <- forkIO $ withConn $ \conn -> do
    forM_ (Map.keys listeners) $ \k -> execute_ conn $ fromString $ "LISTEN " <> show k
    forever $ do
      PG.Notification _ channel message <- PG.getNotification conn
      case Map.lookup channel listeners of
        Nothing -> putStrLn $ "listenDB: received message from unknown channel: " <> show channel
        Just l -> do
          translation <- withConn $ (runDbConn $ tableListenerGetUpdate l $ LBS.fromStrict message) . Postgresql
          mapM_ (atomically . writeTChan nChan) translation
  return (nChan, killThread daemonThread)
