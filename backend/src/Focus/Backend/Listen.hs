{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, FlexibleContexts, NoMonoLocalBinds, RankNTypes, MultiParamTypeClasses, UndecidableInstances, ConstraintKinds, LambdaCase, AllowAmbiguousTypes, PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Focus.Backend.Listen where

import Focus.Account
import Focus.Api
import Focus.AppendMap
import Focus.Backend.Schema.TH
import Focus.Schema
import Focus.Request
import Focus.Sign
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

insertAndNotify :: (PersistBackend m, DefaultKey a ~ AutoKey a, DefaultKeyId a, PersistEntity a, ToJSON a, ToJSON (IdData a)) => a -> m (Id a)
insertAndNotify t = do
  tid <- liftM toId $ insert t
  notifyEntity NotificationType_Insert tid t
  return tid

insertByAllAndNotify :: (PersistBackend m, DefaultKey a ~ AutoKey a, DefaultKeyId a, PersistEntity a, ToJSON a, ToJSON (IdData a)) => a -> m (Maybe (Id a))
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

notifyEntity :: (PersistBackend m, PersistEntity a, ToJSON (IdData a), ToJSON a) => NotificationType -> Id a -> a -> m ()
notifyEntity nt aid _ = notifyEntityId nt aid

notifyEntityWithBody :: forall a m. (PersistBackend m, PersistEntity a, ToJSON (IdData a), ToJSON a) => NotificationType -> Id a -> a -> m ()
notifyEntityWithBody nt aid a = do
  let proxy = undefined :: proxy (PhantomDb m)
      cmd = "NOTIFY " <> updateChannel <> ", ?"
  _ <- executeRaw False cmd [PersistString $ T.unpack $ decodeUtf8 $ LBS.toStrict $ encode (nt, entityName $ entityDef proxy (undefined :: a), (aid, a))]
  return ()

notifyEntityId :: forall a m. (PersistBackend m, PersistEntity a, ToJSON (IdData a)) => NotificationType -> Id a -> m ()
notifyEntityId nt aid = do
  let proxy = undefined :: proxy (PhantomDb m)
  let cmd = "NOTIFY " <> updateChannel <> ", ?"
  _ <- executeRaw False cmd [PersistString $ T.unpack $ decodeUtf8 $ LBS.toStrict $ encode (nt, entityName $ entityDef proxy (undefined :: a), aid)]
  return ()

handleListen :: forall m m' rsp rq notification vs vp token.
                (MonadSnap m, MonadIO m, MonadListenDb m', ToJSON rsp, FromJSON rq, FromJSON notification,
                 FromJSON vs, ToJSON vp, Monoid vp, FromJSON token, ToJSON token, Ord token, Monoid vs)
             => (forall x. m' x -> IO x) -- runGroundhog
             -> TChan notification -- chan
             -> AppendMap token vs -- vsMap0
             -> (token -> vs -> vs -> m' vp) -- getView
             -> (token -> vs -> notification -> m' (Maybe vp)) -- getPatch
             -> (token -> vs -> vs -> m' vp) -- onViewSelectorChange
             -> (rq -> IO rsp) -- processRequest
             -> m () 
handleListen runGroundhog chan vsMap0 getView getPatch onViewSelectorChange processRequest = ifTop $ do
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
        Left s -> sendDataMessage conn . wrapper . encodeR $ Left (mconcat ["error: ", s, "\n", "received: ", show r])
        Right (WebSocketData_Api rid rq) -> sender rid $ processRequest rq
        Right (WebSocketData_Listen vsMap) -> do
          vsMapOld <- readIORef vsRef
          let vsDiff = diffVsMaps vsMap vsMapOld
          atomicModifyIORef vsRef (const (vsMap, ()))
          viewMap <- fmap (_Wrapped %~ Map.mapMaybe id) . runGroundhog $ do
            iforM vsDiff $ \token -> \case
              -- Logout
              That vsOld -> fmap Just $ onViewSelectorChange token mempty vsOld -- TODO Should we run getView again on logout?
              -- Login
              This vsNew -> fmap Just $ liftM2 (<>) (onViewSelectorChange token vsNew mempty) (getView token vsNew mempty)
              -- Update for a logged in user
              These vsNew vsOld -> fmap Just $ liftM2 (<>) (onViewSelectorChange token vsNew vsOld) (getView token vsNew vsOld)
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

