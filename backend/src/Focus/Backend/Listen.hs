{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
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

module Focus.Backend.Listen ( ViewListener, MonadListenDb, NotificationType(..), getPatchesForTokens
                            , getViewsForTokens, handleListen, handleRequests, insertAndNotify
                            , insertByAllAndNotify, insertByAllAndNotifyWithBody, listenDB
                            , makeViewListener, notifyEntity, notifyEntityId, notifyEntityWithBody
                            , notifyEntityWithBody', getPatchesFor, updateAndNotify
                            , notificationListener, updateAndNotifyWithBody
                            , getSchemaName
                            ) where

import Focus.Account
import Focus.Api
import Focus.AppendMap (AppendMap (..))
import qualified Focus.AppendMap as AppendMap
import Focus.Backend.DB
import Focus.Backend.Schema.TH
import Focus.Request
import Focus.Schema
import Focus.Sign
import Focus.WebSocket

import Control.Concurrent
import Control.Concurrent.STM (TChan, atomically, dupTChan, readTChan, newBroadcastTChanIO, writeTChan)
import Control.Exception (handle, try, SomeException, displayException, throwIO)
import Control.Lens
import Control.Monad.Cont
import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict hiding (state, get)
import Control.Monad.Writer
import Data.Aeson
import Data.Align
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.List.Split
import Data.Pool
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.These
import Data.Traversable
import Database.Groundhog
import qualified Database.Groundhog.Core as GH
import Database.Groundhog.Core hiding (Update)
import qualified Database.Groundhog.Expression as GH
import Database.Groundhog.Generic
import Database.Groundhog.Instances ()
import Database.Groundhog.Postgresql
import Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import Network.WebSockets as WS
import Network.WebSockets.Connection as WS
import Network.WebSockets.Snap
import Reflex (FunctorMaybe(..), ffor)
import Snap

import Debug.Trace (trace)

type MonadListenDb m = (PersistBackend m, SqlDb (PhantomDb m))

data NotificationType = NotificationType_Insert | NotificationType_Update | NotificationType_Delete
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

makeJson ''NotificationType

data Connections alignedVs vs vp state = Connections
  { _connections_connState :: AppendMap ThreadId (vp -> IO (), MVar (vs, state))
  , _connections_alignedViewSelector :: alignedVs (AppendMap ThreadId ())
  }
makeLenses ''Connections

-- | Contains synchronization tools for websockets to communicate with listener thread.
data ViewListener alignedVs vs vp state = ViewListener
  { _viewListener_connections :: MVar (Connections alignedVs vs vp state)
  , _viewListener_thread :: ThreadId
  }

class NotificationListener f where
  notificationListener :: FromJSON a -- ^ Notifications from the database are serialized as JSON
                       => f (Pool Postgresql) -- ^ DB pool
                       -> IO (TChan a, IO ()) -- ^ Pair of notifications and finalizer action for this listener

instance NotificationListener Identity where
  notificationListener (Identity db) = listenDB "public" (\f -> withResource db $ \(Postgresql conn) -> f conn)

instance NotificationListener WithSchema where
  notificationListener (WithSchema (SchemaName schema) db) = listenDB schema (\f -> withResource db $ \(Postgresql conn) -> f conn)

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

updateAndNotifyWithBody :: (ToJSON (IdData a), GH.Expression (PhantomDb m) (RestrictionHolder v c) (DefaultKey a), PersistEntity v, PersistEntity a, PersistBackend m, GH.Unifiable (AutoKeyField v c) (DefaultKey a), DefaultKeyId a, _)
                        => Id a
                        -> [GH.Update (PhantomDb m) (RestrictionHolder v c)]
                        -> m ()
updateAndNotifyWithBody tid dt = do
  update dt (AutoKeyField ==. fromId tid)
  Just t <- get $ fromId tid
  notifyEntityWithBody NotificationType_Update tid t

notifyEntity :: (PersistBackend m, PersistEntity a, ToJSON (IdData a)) => NotificationType -> Id a -> a -> m ()
notifyEntity nt aid _ = notifyEntityId nt aid

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
notifyEntityWithBody' nt aid b = do
  schemaName <- getSchemaName
  let proxy = undefined :: proxy (PhantomDb m)
      cmd = "NOTIFY " <> schemaName <> ", ?"
  _ <- executeRaw False (fromString cmd) [PersistString $ T.unpack $ decodeUtf8 $ LBS.toStrict $ encode (nt, entityName $ entityDef proxy (undefined :: a), (aid, b))]
  return ()

notifyEntityWithBody :: (PersistBackend m, PersistEntity a, ToJSON (IdData a), ToJSON a) => NotificationType -> Id a -> a -> m ()
notifyEntityWithBody = notifyEntityWithBody'

notifyEntityId :: forall a m. (PersistBackend m, PersistEntity a, ToJSON (IdData a)) => NotificationType -> Id a -> m ()
notifyEntityId nt aid = do
  schemaName <- getSchemaName
  let proxy = undefined :: proxy (PhantomDb m)
      cmd = "NOTIFY " <> schemaName <> ", ?"
  _ <- executeRaw False (fromString cmd) [PersistString $ T.unpack $ decodeUtf8 $ LBS.toStrict $ encode (nt, entityName $ entityDef proxy (undefined :: a), aid)]
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
makeViewListener :: forall notification alignedVs vs vp state. Align alignedVs
                 => TChan notification -- chan
                 -> (forall t. (Traversable t, Align t, FunctorMaybe t)
                             => notification
                             -> alignedVs (t ())
                             -> t (vs, state)
                             -> IO (t (Maybe vp, state)))
                 -> IO (ViewListener alignedVs vs vp state)
makeViewListener chan getPatches = do
  connections <- newMVar $ Connections { _connections_connState = AppendMap.empty
                                       , _connections_alignedViewSelector = nil
                                       }
  changes <- atomically $ dupTChan chan

  thread <- forkIO $ forever $ handle logNotificationError $ do
    notification <- atomically $ readTChan changes

    withMVar connections $ \currentConnections -> do
      revise (_connections_connState currentConnections) $ \selectors -> do
        let alignedVs = _connections_alignedViewSelector currentConnections
        (patches :: AppendMap ThreadId (Maybe vp, state)) <- getPatches notification alignedVs (fmap snd selectors)
        fmap (fmapMaybe id) . forM (align selectors patches) $ \case
          These (send', (vs, _)) (Just patch, newState) -> do send' patch; return . Just $ (vs, newState)
          These (_,     (vs, _)) (Nothing,    newState) -> return . Just $ (vs, newState)
          _ -> return Nothing
      return ()

  return $ ViewListener { _viewListener_connections = connections
                        , _viewListener_thread = thread
                        }
  where
    logNotificationError :: SomeException -> IO ()
    logNotificationError e = putStrLn $ "Focus.Backend.Listen makeViewListener exception: " <> (show e)

handleListen :: forall m m' rsp rq alignedVs vs vp state.
                ( MonadSnap m, ToJSON rsp, FromJSON rq
                , FromJSON vs, ToJSON vp, Monoid vs
                , Align alignedVs, FunctorMaybe alignedVs)
             => (vs -> state -> m' ()) -- connectionCloseHook
             -> ((T.Text -> IO ()) -> IO ())
             -> (forall x. m' x -> IO x) -- runGroundhog
             -> (vs -> alignedVs ()) -- alignViewSelector
             -> ViewListener alignedVs vs vp state
             -> vs
             -> state
             -> (vs -> vs -> StateT state m' vp) -- getView
             -> (rq -> IO rsp) -- processRequest
             -> m ()
handleListen connectionCloseHook connectionOpenHook runGroundhog alignViewSelector viewListener vs0 state0 getView processRequest = runWebSocketsSnap $ \pc -> do
  let connections = _viewListener_connections viewListener
  conn <- acceptRequest pc
  let send' = sendTextData conn . encodeR . Right . WebSocketData_Listen

  threadId <- myThreadId

  let allocate :: IO (MVar (vs, state))
      allocate = do
        stateRef <- newMVar (vs0, state0)
        modifyMVar_ connections $ \currentConnections -> do
          let initialAlignment (This ()) = AppendMap.singleton threadId ()
              initialAlignment (That users) = users
              initialAlignment (These () users) = AppendMap.insert threadId () users
              -- ^ Add the initial view selector with the aligned view selector
          return $ currentConnections & connections_connState           %~ AppendMap.insert threadId (send', stateRef)
                                      & connections_alignedViewSelector %~ alignWith initialAlignment (alignViewSelector vs0)
        return stateRef
      release :: MVar (vs, state) -> IO ()
      release stateRef = do
        modifyMVar_ connections $ \currentConnections -> do
          tryReadMVar stateRef >>= \case
            Just (vs, state) -> runGroundhog $ connectionCloseHook vs state
            Nothing -> return () -- TODO: The stateRef shouldn't be empty when the connection closes. This indicates an error.
          return $ currentConnections & connections_connState           %~ AppendMap.delete threadId
                                      & connections_alignedViewSelector %~ fmapMaybe (AppendMap.nonEmptyDelete threadId)
  bracket allocate release $ \stateRef -> do
    vpInit <- modifyMVar stateRef $ \(vsInit, stateInit) -> do
      -- NB: newStateInit is forced to be strict below to prevent a buildup of thunks in the IORef
      (vpInit, !newStateInit) <- handle (\(e :: SomeException) -> print e >> throwIO e) $
        runGroundhog (runStateT (getView vsInit mempty) stateInit)
      return ((vsInit, newStateInit), vpInit)
    send' vpInit
    let sender act wrapper wsd = do
          er <- try act
          sendDataMessage conn . wrapper . encodeR . Right . wsd $ case er of
            Left (se :: SomeException) -> Left (displayException se)
            Right rsp -> Right rsp
    let sender' wrapper wsd = sendDataMessage conn . wrapper . encodeR . Right $ (WebSocketData_Version wsd)
    connectionOpenHook (sender' WS.Text)

    let handleConnectionException = handle $ \e -> case e of
          ConnectionClosed -> return ()
          CloseRequest _ _ -> print e >> WS.pendingStreamClose pc >> throwIO e
          _ -> do putStr "Exception: " >> print e
                  throwIO e
    handleConnectionException $ forever $ do
      dm <- receiveDataMessage conn
      let (wrapper, r) = case dm of
            WS.Text r' -> (WS.Text, r')
            WS.Binary r' -> (WS.Text, r')

      case eitherDecode' r of
        Left s -> sendDataMessage conn . wrapper . encodeR $ Left (mconcat ["error: ", s, "\n", "received: ", show r])
        Right (WebSocketData_Version _) -> do
          putStrLn "Shouldn't be receiving version from frontend..."
          return ()
        Right (WebSocketData_Api rid rq) -> sender (processRequest rq) wrapper (WebSocketData_Api rid)
        Right (WebSocketData_Listen vs) -> do
          -- Acquire connections
          vp <- modifyMVar connections $ \currentConnections -> do
            -- Now that connections are acquired, we can safely acquire the stateRef.
            (vsOld, vp) <- modifyMVar stateRef $ \(vsOld, state) -> do
              -- NB: newState is forced to be strict below to prevent a buildup of thunks in the IORef
              (vp, !newState) <- handle (\(e :: SomeException) -> print e >> throwIO e) $
                runGroundhog (runStateT (getView vs vsOld) state)
              return ((vs, newState), (vsOld, vp))

            -- Return the patch to be sent, and modify the aligned view selector with the new local view selector.
            return (currentConnections & connections_alignedViewSelector %~ diffAlign threadId
                                                                                      (alignViewSelector vsOld)
                                                                                      (alignViewSelector vs)
                   , vp)
          send' vp

 where
   encodeR :: Either String (WebSocketData vp (Either String rsp)) -> LBS.ByteString
   encodeR = encode

-- | Creates a function that iterates the argument function.
getPatchesFor :: (Applicative m, Traversable t)
              => (vs -> StateT state m (Maybe vp))
              -> t (vs, state) -> m (t (Maybe vp, state))
getPatchesFor getPatch selectors = for selectors $
  \(vs, state) -> runStateT (getPatch vs) state

-- | Like 'alignWith', except the function may return 'Maybe'. Using
--   'FunctorMaybe', elements that return 'Nothing' will be removed.
alignWithMaybe :: (Align f, FunctorMaybe f)
               => (These a b -> Maybe c) -> f a -> f b -> f c
alignWithMaybe f a b = fmapMaybe f $ align a b

-- | Diff two structures, and apply the diff to a key in a third structure.
diffAlign :: (Align f, FunctorMaybe f, Ord token)
          => token
          -> f a -- ^ Remove these
          -> f b -- ^ Add these
          -> f (AppendMap token b) -- ^ Modify these
          -> f (AppendMap token b)
diffAlign token remove add xs = let
  -- Remove elements that are only in the `remove` structure
  diffAlignment (This _) = AppendMap.nonEmptyDelete token
  -- Add elements that are only in the `add` structure
  diffAlignment (That x) = Just . AppendMap.insert token x
  -- Overwrite old elements with new elements
  diffAlignment (These _ x) = Just . AppendMap.insert token x

  diff = alignWith diffAlignment remove add

  -- nothing to update; start with empty
  realignment (This updateXs) = updateXs AppendMap.empty
  -- No update to perform
  realignment (That xs') = Just xs'
  -- Perform update
  realignment (These updateXs xs') = updateXs xs'

  -- Use 'alignWithMaybe' to remove empty maps
  in alignWithMaybe realignment diff xs

getViewsForTokens :: (Monad m, Align (AppendMap token), Monoid vs, Default state)
                  => (token -> vs -> vs -> StateT state m vp)
                  -> (token -> m ()) -- ^ login hook
                  -> (token -> m ()) -- ^ logout hook
                  -> AppendMap token vs
                  -> AppendMap token vs
                  -> StateT (AppendMap token state) m (AppendMap token vp)
getViewsForTokens getView loginHook logoutHook vs vsOld = do
  states <- State.get

  -- TODO: Do something better than expecting these two to come aligned
  let stateWithSelectors (This vsNew) = Just (vsNew, def) -- Login needs new state
      stateWithSelectors (That _) = Nothing -- Logout removes state
      stateWithSelectors (These vsNew state) = Just (vsNew, state)
      vs' = fmapMaybe id $ alignWith stateWithSelectors vs states

  viewMap <- lift $ fmap (fmapMaybe id) $ ifor (align vs' vsOld) $ \token -> \case
    -- Logout
    That _ -> Nothing <$ logoutHook token
    -- Login
    This (vsNew, state) -> loginHook token >> Just <$> runStateT (getView token vsNew mempty) state
    -- Update for a logged in user
    These (vsNew, state) vsOld' -> Just <$> runStateT (getView token vsNew vsOld') state

  put $ fmap snd viewMap
  return $ fmap fst viewMap

-- | Get the patches for authenticated view selectors.
getPatchesForTokens :: (Functor m, Ord token, Traversable t, Align t, FunctorMaybe t, Align alignedVs, Default state)
                    => (forall t'. (Traversable t', Align t', FunctorMaybe t') =>
                                   alignedVs (t' ()) -> t' (vs, state, token) -> m (t' (Maybe vp, state)))
                    -> alignedVs (t ())
                    -> t (AppendMap token vs, AppendMap token state)
                    -> m (t (Maybe (AppendMap token vp), AppendMap token state))
getPatchesForTokens getPatches alignedVs selectors =
  -- TODO: Do something better than expecting these two to come aligned.
  let stateWithSelectors (This vs) = trace "Warning: missing state in getPatchesForTokens" $ Just (vs, def)
      stateWithSelectors (That _state) = trace "Warning: missing view selector in getPatchesForTokens" Nothing
      stateWithSelectors (These vs state) = Just (vs, state)
      selectors' = AlignCompose $ ffor selectors (imap (\token (vs, state) -> (vs, state, token))
                                                  . fmapMaybe id . uncurry (alignWith stateWithSelectors))
      separateStateAndPatches vps = (AppendMap.mapMaybeNoNull fst vps, fmap snd vps)
      tokenizeAlignedVs (This (vs, _)) = Just $ fmap (const ()) vs
      tokenizeAlignedVs (That ()) = Nothing
      tokenizeAlignedVs (These (vs, _) ()) = Just $ fmap (const ()) vs
  in fmap separateStateAndPatches . getAlignCompose <$>
     getPatches (fmap (AlignCompose . fmapMaybe id . alignWith tokenizeAlignedVs selectors) alignedVs) selectors'

-- | Compose an instance of 'FunctorMaybe' and 'Align' with
--   'AppendMap'. 'FunctorMaybe's don't compose by nature. You have
--   to define a curated instance to get the desired effect.
--
--   Note: If there were a class for endofunctors on @Kleisli Maybe@,
--   'AppendMap' would fit like this:
--
-- @
-- instance EndoFunctorMaybe (AppendMap token) where
--   maybeMapMaybe :: (a -> Maybe b) -> AppendMap token a -> Maybe (AppendMap token b)
--   maybeMapMaybe f as = let bs = fmapMaybe f as
--     in if null bs
--        then Nothing
--        else Just bs
-- @
--
--   This class of functors does compose with 'FunctorMaybe', and is
--   hard-coded into this implementation, as there is no class for
--   these Kleisli endofunctors.
--
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

listenDB :: FromJSON a => Text -> (forall x. (PG.Connection -> IO x) -> IO x) -> IO (TChan a, IO ())
listenDB schema withConn' = do
  nChan <- newBroadcastTChanIO
  daemonThread <- forkIO $ withConn' $ \conn -> do
    let cmd = "LISTEN " <> T.unpack schema
    execute_ conn $ fromString cmd
    forever $ do
      PG.Notification _ channel message <- PG.getNotification conn
      case channel of
        _ | channel == encodeUtf8 schema -> do
          case decodeValue' $ LBS.fromStrict message of
            Just a -> atomically $ writeTChan nChan a
            _ -> putStrLn $ "listenDB: Could not parse message on updates channel: " <> show message
        _ -> putStrLn $ "listenDB: Received a message on unexpected channel: " <> show channel
  return (nChan, killThread daemonThread)

handleRequests :: forall h m pub priv f. (Monad m)
               => (forall x. ToJSON x => h x -> m Value) -- Runs request and turns response into JSON
               -> (forall x. pub f x -> h x) -- Public request handler
               -> (forall x. Signed (AuthToken f) -> priv f x -> h x) -- Private request handler
               -> SomeRequest (ApiRequest f pub priv) -- Api Request
               -> m Value -- JSON response
handleRequests runRequest fpub fpriv request = case request of
  SomeRequest req -> do
    rsp <- case req of
      ApiRequest_Public r -> runRequest (fpub r)
      ApiRequest_Private token r -> runRequest (fpriv token r)
    return rsp
