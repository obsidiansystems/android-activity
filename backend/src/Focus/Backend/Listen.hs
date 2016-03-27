{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, FlexibleContexts, NoMonoLocalBinds, RankNTypes, MultiParamTypeClasses, UndecidableInstances, ConstraintKinds, LambdaCase #-}
module Focus.Backend.Listen where

import Focus.AppendMap
import Focus.Backend.Schema.TH
import Focus.Schema
import Focus.Request
import Focus.WebSocket

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (handle, try, SomeException, displayException)
import Control.Lens
import Control.Monad.Writer
import Data.Aeson
import qualified Data.Map as Map
import Data.IORef
import Data.Pool
import Data.String
import Data.Text.Encoding
import Data.These
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

notifyEntityId :: forall a m. (PersistBackend m, PersistEntity a, ToJSON (IdData a)) => Id a -> m ()
notifyEntityId aid = do
  let proxy = undefined :: proxy (PhantomDb m)
  let cmd = "NOTIFY " <>  updateChannel <> ", ?"
  _ <- executeRaw False cmd [PersistString $ T.unpack $ decodeUtf8 $ LBS.toStrict $ encode (entityName $ entityDef proxy (undefined :: a), aid)]
  return ()

notifyDerived :: (PersistBackend m, DerivedEntity a, ToJSON (IdData (DerivedEntityHead a)), ToJSON (DerivedEntityHead a)) => Id a -> a -> m ()
notifyDerived aid _ = notifyDerivedId aid

notifyDerivedId :: forall a m. (PersistBackend m, DerivedEntity a, ToJSON (IdData (DerivedEntityHead a))) => Id a -> m ()
notifyDerivedId aid = do
  let proxy = undefined :: proxy (PhantomDb m)
      cmd = "NOTIFY " <> updateChannel <> ", ?"
  _ <- executeRaw False cmd [PersistString $ T.unpack $ decodeUtf8 $ LBS.toStrict $ encode (entityName $ entityDef proxy (undefined :: DerivedEntityHead a), fromDerivedId aid)]
  return ()

handleListen :: forall m m' rsp rq notification vs vp token.
                (MonadSnap m, MonadIO m, MonadListenDb m', ToJSON rsp, FromJSON rq, FromJSON notification,
                 FromJSON vs, ToJSON vp, Monoid vp, FromJSON token, ToJSON token, Ord token, Monoid vs)
             => (forall x. m' x -> IO x) -- runGroundhog
             -> TChan notification -- chan
             -> AppendMap token vs -- vsMap0
             -> (vs -> vs -> vs) -- diffViewSel
             -> (token -> vs -> vs -> m' vp) -- getView
             -> (token -> vs -> notification -> m' (Maybe vp)) -- getPatch
             -> (token -> vs -> vs -> m' vp) -- onViewSelectorChange
             -> (rq -> IO rsp) -- processRequest
             -> m () 
handleListen runGroundhog chan vsMap0 diffViewSel getView getPatch onViewSelectorChange processRequest = ifTop $ do
  changes <- liftIO . atomically $ dupTChan chan
  vsRef <- liftIO $ newIORef vsMap0
  runWebSocketsSnap $ \pc -> do
    conn <- acceptRequest pc
    let send' = sendTextData conn . encodeR . Right . WebSocketData_Listen
    senderThread <- forkIO $ do
      vsMapInit <- readIORef vsRef
      send' =<< runGroundhog (iforM vsMapInit (\token vs -> getView token vs vs))
      forever $ do
        change <- atomically $ readTChan changes
        vsMap <- readIORef vsRef
        mpMap <- fmap (_Wrapped %~ Map.mapMaybe id) . runGroundhog $ iforM vsMap (\token vs -> getPatch token vs change)
        case Map.null (mpMap ^. _Wrapped) of
          True -> return ()
          False -> send' mpMap
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
        Left s -> sendDataMessage conn . wrapper . encodeR $ Left s
        Right (WebSocketData_Api rid rq) -> sender rid $ processRequest rq
        Right (WebSocketData_Listen vsMap) -> do
          vsMapOld <- readIORef vsRef
          let vsDiff = diffVsMaps vsMap vsMapOld
          atomicModifyIORef vsRef (const (vsMap, ()))
          viewMap <- fmap (_Wrapped %~ Map.mapMaybe id) . runGroundhog $ do
            iforM vsDiff $ \token -> \case
              That vsOld -> fmap Just $ onViewSelectorChange token mempty vsOld
              This vs -> fmap Just $ liftM2 (<>) (onViewSelectorChange token vs mempty) (getView token vs vs)
              These vs vsOld -> fmap Just $ liftM2 (<>) (onViewSelectorChange token vs vsOld) (getView token vs (diffViewSel vs vsOld))
          send' viewMap
    killThread senderThread
    vsMap <- readIORef vsRef
    runGroundhog (iforM_ vsMap $ \token vs -> onViewSelectorChange token vs mempty)
 where encodeR :: Either String (WebSocketData token vp (Either String rsp)) -> LBS.ByteString
       encodeR = encode
       diffVsMaps (AppendMap m0) (AppendMap m1) = AppendMap $ Map.mergeWithKey (\_ a b -> Just (These a b)) (fmap This) (fmap That) m0 m1

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
