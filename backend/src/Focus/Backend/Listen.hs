{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Focus.Backend.Listen ( ViewListener (..), MonadListenDb, NotificationType(..), getPatchesForTokens
                            , getViewsForTokens, handleListen, handleRequests, insertAndNotify
                            , insertByAllAndNotify, insertByAllAndNotifyWithBody, listenDB
                            , NotifyMessage (..)
                            , makeViewListener, notifyEntities, notifyEntityId, notifyEntityWithBody
                            , notifyEntityWithBody', getPatchesFor, updateAndNotify
                            , notificationListener, notificationListenerWithSession, updateAndNotifyWithBody
                            , getSchemaName
                            ) where

import Focus.Api
import Focus.AppendMap (AppendMap (..))
import qualified Focus.AppendMap as AppendMap
import Focus.Backend.DB
import Focus.Backend.Schema.TH
import Focus.Request
import Focus.Schema
import Focus.WebSocket

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Lens hiding (view, (<.))
import Control.Monad.Cont
import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict hiding (state, get)
import Control.Monad.Writer
import Data.Aeson
import Data.Align
import qualified Data.ByteString.Lazy as LBS
import Data.List.Split
import Data.Pool
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.These
import Data.Traversable
import Database.Groundhog
import qualified Database.Groundhog.Core as GH
import Database.Groundhog.Core hiding (Update)
import qualified Database.Groundhog.Expression as GH
import Database.Groundhog.Instances ()
import Database.Groundhog.Postgresql
import Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import GHC.Generics
import Network.WebSockets as WS
import Network.WebSockets.Connection as WS
import Network.WebSockets.Snap
import Reflex (FunctorMaybe(..), ffor)
import Snap
import System.IO

import Debug.Trace (trace)

import Focus.Backend.Listen.Session

-- Noisy backend logging
--import qualified Data.ByteString.Lazy.Char8 as LBSC8
--import Data.Time.Clock
--import Data.Time.Format

type MonadListenDb m = (PersistBackend m, SqlDb (PhantomDb m))

data NotifyMessage a
   = NotifyMessage { _notifyMessage_schemaName :: SchemaName
                   , _notifyMessage_notificationType :: NotificationType
                   , _notifyMessage_entityName :: String
                   , _notifyMessage_value :: a
                   }
  deriving (Show, Read, Eq, Generic)

instance FromJSON a => FromJSON (NotifyMessage a)
instance ToJSON a => ToJSON (NotifyMessage a)

data NotificationType = NotificationType_Insert
                      | NotificationType_Update
                      | NotificationType_Delete
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

makeJson ''NotificationType

data Connections vs vp state = Connections
  { _connections_connState :: AppendMap ThreadId (vp -> IO (), MVar (vs, state))
  }
makeLenses ''Connections

-- | Contains synchronization tools for websockets to communicate with listener thread.
data ViewListener vs vp state = ViewListener
  { _viewListener_connections :: TVar (AppendMap SchemaName (TMVar (Connections vs vp state)))
  , _viewListener_thread :: ThreadId
  }

notificationListener :: FromJSON a
                     => Pool Postgresql
                     -> IO (TChan a, IO ())
notificationListener db = listenDB (\f -> withResource db $ \(Postgresql conn) -> f conn)

notificationListenerWithSession :: FromJSON a -- Notifications from the database are serialized as JSON
                                => Pool Postgresql -- ^ DB pool
                                -> (Id Session -> FocusPersist ()) -- ^ Clean up sessions that have disappeared
                                -> IO (TChan a, Id Session, IO ()) -- ^ Tuple of (notifications, session id, and  finalizer action)
notificationListenerWithSession db cleanup = do
  sid <- runDb (Identity db) $ fmap toId . insert . flip Session True =<< getTime
  killHb <- heartbeat db sid
  killSupervise <- superviseSessions db cleanup
  (nchan, nfinalize)  <- listenDB (\f -> withResource db $ \(Postgresql conn) -> f conn)
  return (nchan, sid, killHb >> killSupervise >> nfinalize)

insertAndNotify :: (PersistBackend m, DefaultKey a ~ AutoKey a, DefaultKeyId a, PersistEntity a, ToJSON (IdData a)) => a -> m (Id a)
insertAndNotify t = do
  tid <- liftM toId $ insert t
  notifyEntityId NotificationType_Insert tid
  return tid

insertByAllAndNotify :: (PersistBackend m, DefaultKey a ~ AutoKey a, DefaultKeyId a, PersistEntity a, ToJSON (IdData a)) => a -> m (Maybe (Id a))
insertByAllAndNotify t = do
  etid <- liftM (fmap toId) $ insertByAll t
  case etid of
    Left _ -> return Nothing
    Right tid -> notifyEntityId NotificationType_Insert tid >> return (Just tid)

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

updateAndNotifyWithBody :: (ToJSON (IdData a), GH.Expression (PhantomDb m) (RestrictionHolder v c) (DefaultKey a), PersistEntity v, PersistEntity a, PersistBackend m, GH.Unifiable (AutoKeyField v c) (DefaultKey a), DefaultKeyId a, _)
                        => Id a
                        -> [GH.Update (PhantomDb m) (RestrictionHolder v c)]
                        -> m ()
updateAndNotifyWithBody tid dt = do
  update dt (AutoKeyField ==. fromId tid)
  Just t <- get $ fromId tid
  notifyEntityWithBody NotificationType_Update tid t

getSchemaName :: PersistBackend m
              => m String
getSchemaName =  do
  searchPath <- getSearchPath
  let searchPathComponents = wordsBy (==',') searchPath
      schemaName = case searchPathComponents of
       (x:_:_:_) -> x
       _ -> "public"
  return schemaName

notifyEntityWithBody' :: forall a b m. (PersistBackend m, PersistEntity a, ToJSON (IdData a), ToJSON b) => NotificationType -> Id a -> b -> m ()
notifyEntityWithBody' nt aid b = notifyEntities nt (b, aid)

notifyEntityWithBody :: (PersistBackend m, PersistEntity a, ToJSON (IdData a), ToJSON a) => NotificationType -> Id a -> a -> m ()
notifyEntityWithBody = notifyEntityWithBody'

notifyEntityId :: forall a m. (PersistBackend m, PersistEntity a, ToJSON (IdData a)) => NotificationType -> Id a -> m ()
notifyEntityId nt aid = notifyEntities nt (Identity aid)

notifyEntities :: forall a f m. (PersistBackend m, PersistEntity a, ToJSON (f (Id a))) => NotificationType -> f (Id a) -> m ()
notifyEntities nt aid = do
  schemaName <- getSchemaName
  let proxy = undefined :: proxy (PhantomDb m)
      cmd = "NOTIFY " <> notifyChannel <> ", ?"
      notification = NotifyMessage { _notifyMessage_schemaName = SchemaName . T.pack $ schemaName
                                   , _notifyMessage_notificationType = nt
                                   , _notifyMessage_entityName = entityName $ entityDef proxy (undefined :: a)
                                   , _notifyMessage_value = aid
                                   }
  _ <- executeRaw False cmd [PersistString $ T.unpack $ decodeUtf8 $ LBS.toStrict $ encode notification]
  return ()

-- An elaborate control structure to do something a bit silly: we're modifying all the state MVars for all the connections at once,
-- locking everything while we compute all the patches for a DB notification.
-- AppendMap ThreadId (vp -> IO (), MVar (vs, state)) is the actual type of the first argument in usage.
revise :: forall k s a. (Ord k) => AppendMap k (s, MVar a) -> (AppendMap k (s,a) -> IO (AppendMap k a)) -> IO ()
revise m body = revise' (AppendMap.toList m) (AppendMap.empty) >> return ()
  where
    revise' :: [(k, (s, MVar a))] -> AppendMap k (s,a) -> IO (AppendMap k a)
    revise' [] n = body n
    revise' ((k,(s,v)):ms) n = modifyMVar v $ \a -> do
      n' <- revise' ms (AppendMap.insert k (s,a) n)
      case AppendMap.lookup k n' of
        Nothing -> return (a,n')
        Just a' -> return (a',n')

-- | Given a channel of notifications and a function for traversing view selectors to generate patches,
--   starts a thread that listens for notifications, collects view selectors from websockets,
--   and sends patches down to the user.
makeViewListener :: forall vs vp state a.
                    TChan (NotifyMessage a) -- chan
                 -> (forall t. (Traversable t, Align t, FunctorMaybe t)
                             => NotifyMessage a
                             -> t (vs, state)
                             -> IO (t (Maybe vp, state)))
                 -> IO (ViewListener vs vp state)
makeViewListener chan getPatches = do
  connections <- newTVarIO (AppendMap.empty :: AppendMap SchemaName (TMVar (Connections vs vp state)))
  changes <- atomically $ dupTChan chan

  thread <- forkIO $ forever $ handle logNotificationError $ do
    n@(NotifyMessage schema _ _ _) <- atomically $ readTChan changes
    -- Insert schema into connections map if it isn't there
    schemaConn <- ensureSchemaViewListenerExists schema connections
    bracket (atomically $ takeTMVar schemaConn)
            (atomically . putTMVar schemaConn)
            (go n)
  return $ ViewListener { _viewListener_connections = connections
                        , _viewListener_thread = thread
                        }
  where
    -- TODO remove empty schema keys from the connections map
    go :: NotifyMessage a -> Connections vs vp state -> IO ()
    go notification conns = revise (_connections_connState conns) $ \selectors -> do
      patches <- getPatches notification (fmap snd selectors)
      fmap (fmapMaybe id) . forM (align selectors patches) $ \case
        These (send', (vs, _)) (Just patch, newState) -> do send' patch; return . Just $ (vs, newState)
        These (_,     (vs, _)) (Nothing,    newState) -> return . Just $ (vs, newState)
        _ -> return Nothing
    logNotificationError :: SomeException -> IO ()
    logNotificationError e = hPutStrLn stderr $ "Focus.Backend.Listen makeViewListener exception: " <> (show e)

ensureSchemaViewListenerExists :: (Ord k)
                               => k
                               -> TVar (AppendMap k (TMVar (Connections vs vp state)))
                               -> IO (TMVar (Connections vs vp state))
ensureSchemaViewListenerExists schema connections = atomically $ do
  connsBySchema <- readTVar connections
  case AppendMap.lookup schema connsBySchema of
    Nothing -> do
      newSchemaConn <- newTMVar $
        Connections { _connections_connState = AppendMap.empty
                    }
      modifyTVar' connections $ AppendMap.insert schema newSchemaConn
      return newSchemaConn
    Just schemaConn -> return schemaConn

handleListen :: forall m m' rsp rq vs vp state.
                ( MonadSnap m, ToJSON rsp, FromJSON rq, ToJSON rq
                , FromJSON vs, ToJSON vs, ToJSON vp, Monoid vs
                )
             => SchemaName
             -> (vs -> state -> m' ()) -- connectionCloseHook
             -> ((T.Text -> IO ()) -> IO ())
             -> (forall x. m' x -> IO x) -- runGroundhog
             -> ViewListener vs vp state
             -> vs
             -> state
             -> (vs -> vs -> StateT state m' vp) -- getView
             -> (state -> rq -> IO rsp) -- processRequest
             -> m ()
handleListen schema connectionCloseHook connectionOpenHook runGroundhog viewListener vs0 state0 getView processRequest = runWebSocketsSnap $ \pc -> do
  connections <- ensureSchemaViewListenerExists schema $ _viewListener_connections viewListener
  conn <- acceptRequest pc
  let send' = sendTextData conn . encodeR . Right . WebSocketData_Listen
      modifyTMVar_ v f = bracketOnError (atomically $ takeTMVar v)
                                        (atomically . putTMVar v)
                                        (atomically . putTMVar v <=< f)
      modifyTMVar v f = bracketOnError (atomically $ takeTMVar v) (atomically . putTMVar v) $ \a -> do
        (a', b) <- f a
        atomically $ putTMVar v a'
        return b

  threadId <- myThreadId

  let allocate = do
        stateRef <- newMVar (vs0, state0)
        modifyTMVar_ connections $ \currentConnections -> do
          return $ currentConnections & connections_connState %~ AppendMap.insert threadId (send', stateRef)
        return stateRef
      release stateRef = do
        modifyTMVar_ connections $ \currentConnections -> do
          tryReadMVar stateRef >>= \case
            Just (vs, state) -> runGroundhog $ connectionCloseHook vs state
            Nothing -> return () -- TODO: The stateRef shouldn't be empty when the connection closes. This indicates an error.
          return $ currentConnections & connections_connState %~ AppendMap.delete threadId
  bracket allocate release $ \stateRef -> do
    vpInit <- modifyMVar stateRef $ \(vsInit, stateInit) -> do
      -- NB: newStateInit is forced to be strict below to prevent a buildup of thunks in the IORef
      (vpInit, !newStateInit) <- handle (\(e :: SomeException) -> print e >> throwIO e) $
        runGroundhog (runStateT (getView vsInit mempty) stateInit)
      return ((vsInit, newStateInit), vpInit)
    send' vpInit
    let sender {- NOISY LOGGING t0 -} act wrapper wsd = do
          er <- try act
          let payload = case er of
                Left (se :: SomeException) -> Left (displayException se)
                Right rsp -> Right rsp
          {- NOISY LOGGING
          t1 <- formattedTimestamp
          LBSC8.putStrLn $ tabSeparated $ [ "RESPONSE", t0, t1, encode payload ] -- -}
          sendDataMessage conn . wrapper . encodeR . Right . wsd $ payload
        sender' wrapper wsd = sendDataMessage conn . wrapper . encodeR . Right $ (WebSocketData_Version wsd)
        handleConnectionException = handle $ \e -> case e of
          ConnectionClosed -> return ()
          CloseRequest _ _ -> print e >> WS.pendingStreamClose pc >> throwIO e
          _ -> do putStr "Exception: " >> print e
                  throwIO e
    connectionOpenHook (sender' WS.Text)
    handleConnectionException $ forever $ do
      dm <- receiveDataMessage conn
      let (wrapper, r) = case dm of
            WS.Text r' -> (WS.Text, r')
            WS.Binary r' -> (WS.Text, r')
          decoded = eitherDecode' r
      {- NOISY LOGGING
      t0 <- formattedTimestamp
      LBSC8.putStrLn $ tabSeparated $ [ "REQUEST", t0, encode decoded ] -- -}
      case decoded of
        Left s -> do
          sendDataMessage conn . wrapper . encodeR $ Left (mconcat ["error: ", s, "\n", "received: ", show r])
        Right (WebSocketData_Version _) -> do
          putStrLn "Shouldn't be receiving version from frontend..."
          return ()
        Right (WebSocketData_Api rid rq) -> do
          (_, s) <- readMVar stateRef
          sender {- NOISY LOGGING t0 -} (processRequest s rq) wrapper (WebSocketData_Api rid)
        Right (WebSocketData_Listen vs) -> do
          -- Acquire connections
          vp <- modifyTMVar connections $ \currentConnections -> do
            -- Now that connections are acquired, we can safely acquire the stateRef.
            vp <- modifyMVar stateRef $ \(vsOld, state) -> do
              -- NB: newState is forced to be strict below to prevent a buildup of thunks in the IORef
              (vp, !newState) <- handle (\(e :: SomeException) -> print e >> throwIO e) $
                runGroundhog (runStateT (getView vs vsOld) state)
              return ((vs, newState), vp)

            -- Return the patch to be sent, and modify the aligned view selector with the new local view selector.
            return (currentConnections
                   , vp)
          {- NOISY LOGGING
          t1 <- formattedTimestamp
          LBSC8.putStrLn $ tabSeparated $ [ "RESPONSE", t0, t1, "LISTEN" ] -- -}
          send' vp

 where
   {- NOISY LOGGING
   tabSeparated = LBSC8.intercalate (LBSC8.singleton '\t')
   formattedTimestamp = liftIO $
     LBSC8.pack . (++"Z") . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) <$> getCurrentTime -- -}
   encodeR :: Either String (WebSocketData vp (Either String rsp)) -> LBS.ByteString
   encodeR = encode

-- | Creates a function that iterates the argument function.
getPatchesFor :: (Applicative m, Traversable t)
              => (vs -> StateT state m (Maybe vp))
              -> t (vs, state)
              -> m (t (Maybe vp, state))
getPatchesFor getPatch selectors = for selectors $
  \(vs, state) -> runStateT (getPatch vs) state

getViewsForTokens :: (Monad m, Align (AppendMap token), Monoid vs)
                  => (token -> vs -> vs -> StateT state m vp)
                  -> (token -> m state) -- ^ login hook
                  -> (token -> state -> m ()) -- ^ logout hook
                  -> AppendMap token vs
                  -> AppendMap token vs
                  -> StateT (AppendMap token state) m (AppendMap token vp)
getViewsForTokens getView loginHook logoutHook vs vsOld = do
  states <- State.get

  let login token newVS = do
        s <- loginHook token
        (view, newState) <- runStateT (getView token newVS mempty) s
        return $ Just (view, newState)
      logout token s = do
        logoutHook token s
        return Nothing

  viewMap <- lift $ fmap (fmapMaybe id) $ iforM (align vs (align vsOld states)) $ \token -> \case
    -- We have an old view selector and state, but no new view selector:
    That (These _ s) -> logout token s
    -- We have old and new view selectors, so no login is necessary:
    These newVS (These oldVS s) -> Just <$> runStateT (getView token newVS oldVS) s
    -- We have a new VS but no state:
    This newVS -> login token newVS

    -- NB: The cases below are impossible
    -- We have a state, but no old or new view selector:
    That (That s) -> logout token s
    -- We have an old view selector but no state:
    That (This _) -> return Nothing
    -- We have a new view selector and state, but no old view selector:
    These newVS (That _) -> login token newVS
    -- We have an old and new view selector but no state:
    These newVS (This _) -> login token newVS

  put $ fmap snd viewMap
  return $ fmap fst viewMap

-- | Get the patches for authenticated view selectors.
getPatchesForTokens :: (Functor m, Ord token, Traversable t, Align t, FunctorMaybe t)
                    => (forall t'. (Traversable t', Align t', FunctorMaybe t') =>
                                   t' (vs, state, token) -> m (t' (Maybe vp, state)))
                    -> t (AppendMap token vs, AppendMap token state)
                    -> m (t (Maybe (AppendMap token vp), AppendMap token state))
getPatchesForTokens getPatches selectors =
  -- TODO: Do something better than expecting these two to come aligned.
  let stateWithSelectors (This _vs) = trace "Warning: missing state in getPatchesForTokens" Nothing
      stateWithSelectors (That _state) = trace "Warning: missing view selector in getPatchesForTokens" Nothing
      stateWithSelectors (These vs state) = Just (vs, state)
      selectors' = AlignCompose $ ffor selectors (imap (\token (vs, state) -> (vs, state, token))
                                                  . fmapMaybe id . uncurry (alignWith stateWithSelectors))
      separateStateAndPatches vps = (AppendMap.mapMaybeNoNull fst vps, fmap snd vps)
  in fmap separateStateAndPatches . getAlignCompose <$>
     getPatches selectors'

--   TODO: Find a better home for this type. It's currently only used
--   in 'getPatchesForTokens', hence this awkward placement.
newtype AlignCompose token t a = AlignCompose { getAlignCompose :: t (AppendMap token a) }
  deriving (Functor, Foldable, Traversable)

instance FunctorMaybe t => FunctorMaybe (AlignCompose token t) where
  fmapMaybe f = AlignCompose . fmapMaybe (AppendMap.mapMaybeNoNull f) . getAlignCompose

instance (Align t, Ord token) => Align (AlignCompose token t) where
  nil = AlignCompose nil
  alignWith f (AlignCompose as) (AlignCompose bs) = AlignCompose $
    alignWith (these (fmap (f . This)) (fmap (f . That)) (alignWith f)) as bs

notifyChannel :: String
notifyChannel = "updates"

listenDB :: FromJSON a => (forall x. (PG.Connection -> IO x) -> IO x) -> IO (TChan a, IO ())
listenDB withConn' = do
  nChan <- newBroadcastTChanIO
  daemonThread <- forkIO $ withConn' $ \conn -> do
    let cmd = "LISTEN " <> notifyChannel
    _ <- execute_ conn $ fromString cmd
    forever $ do
      PG.Notification _ channel message <- PG.getNotification conn
      case channel of
        _ | channel == encodeUtf8 (T.pack notifyChannel) -> do
          case decodeValue' $ LBS.fromStrict message of
            Just a -> atomically $ writeTChan nChan a
            _ -> putStrLn $ "listenDB: Could not parse message on updates channel: " <> show message
        _ -> putStrLn $ "listenDB: Received a message on unexpected channel: " <> show channel
  return (nChan, killThread daemonThread)

handleRequests :: forall h m pub priv f cred. (Monad m)
               => (forall x. ToJSON x => h x -> m Value) -- Runs request and turns response into JSON
               -> (forall x. pub f x -> h x) -- Public request handler
               -> (forall x. cred -> priv f x -> h x) -- Private request handler
               -> SomeRequest (ApiRequest f cred pub priv) -- Api Request
               -> m Value -- JSON response
handleRequests runRequest fpub fpriv request = case request of
  SomeRequest req -> do
    rsp <- case req of
      ApiRequest_Public r -> runRequest (fpub r)
      ApiRequest_Private token r -> runRequest (fpriv token r)
    return rsp
