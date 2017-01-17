{-# LANGUAGE ConstraintKinds, PolyKinds, GADTs, AllowAmbiguousTypes, DefaultSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeFamilies, UndecidableInstances, FunctionalDependencies, RankNTypes, RecursiveDo, ScopedTypeVariables, OverloadedStrings, ExistentialQuantification, LambdaCase #-}
module Focus.JS.App where

import Control.Lens ( (%~), (^?), _Right, iforM)
import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.Types
import Data.Align
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.Constraint
import Data.Foldable
import Data.Functor.Misc
import Data.Dependent.Map (DMap, DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.These
import Focus.Account
import Focus.Api
import Focus.App
import Focus.AppendMap (AppendMap (..))
import qualified Focus.AppendMap as AppendMap
import Focus.JS.WebSocket
import Focus.Request hiding (Some)
import Focus.Sign
import Focus.WebSocket
import qualified Reflex as R
import Reflex.EventWriter
import Reflex.Dom hiding (MonadWidget, webSocket, Request)
import Reflex.Host.Class

newtype QueryT t q m a = QueryT { unQueryT :: StateT [Behavior t q] (EventWriterT t q (ReaderT (Dynamic t (QueryResult q)) m)) a }
  deriving (Functor, Applicative, Monad, MonadException, MonadFix, MonadIO, MonadHold t, MonadSample t, MonadAtomicRef)

runQueryT :: (MonadFix m, Additive q, Group q, Reflex t) => QueryT t q m a -> Dynamic t (QueryResult q) -> m (a, Incremental t (AdditivePatch q))
runQueryT (QueryT a) qr = do
  ((r, bs), es) <- runReaderT (runEventWriterT (runStateT a mempty)) qr
  return (r, unsafeBuildIncremental (foldlM (\b c -> fmap (b <>) $ sample c) mempty bs) (fmapCheap AdditivePatch es))

class (Group q, Additive q, Query q) => MonadQuery t q m | m -> q t where
  tellQueryIncremental :: Incremental t (AdditivePatch q) -> m ()
  askQueryResult :: m (Dynamic t (QueryResult q))
  queryIncremental :: Incremental t (AdditivePatch q) -> m (Dynamic t (QueryResult q))

instance (Monad m, Group q, Additive q, Query q, Reflex t) => MonadQuery t q (QueryT t q m) where
  tellQueryIncremental q = do
    QueryT (modify (currentIncremental q:))
    QueryT (lift (tellEvent (fmapCheap unAdditivePatch (updatedIncremental q))))
  askQueryResult = QueryT ask
  queryIncremental q = do
    tellQueryIncremental q
    r <- askQueryResult
    return $ zipDynWith crop (incrementalToDynamic q) r

tellQueryDyn :: (Reflex t, MonadQuery t q m) => Dynamic t q -> m ()
tellQueryDyn d = tellQueryIncremental $ unsafeBuildIncremental (sample (current d)) $ attachWith (\old new -> AdditivePatch $ new ~~ old) (current d) (updated d)

queryDyn :: (Reflex t, Monad m, MonadQuery t q m) => Dynamic t q -> m (Dynamic t (QueryResult q))
queryDyn q = do
  tellQueryDyn q
  r <- askQueryResult
  return $ zipDynWith crop q r

newtype QueryTLoweredResult t q v = QueryTLoweredResult (v, [Behavior t q])

instance (Reflex t, MonadFix m, Group q, Additive q, Query q, MonadHold t m, MonadAdjust t m) => MonadAdjust t (QueryT t q m) where
  runWithReplace (QueryT a0) a' = do
    ((r0, bs0), r') <- QueryT $ lift $ runWithReplace (runStateT a0 []) $ fmapCheap (flip runStateT [] . unQueryT) a'
    tellQueryIncremental $
      let sampleBs :: forall m'. MonadSample t m' => [Behavior t q] -> m' q
          sampleBs = foldlM (\b a -> fmap (b <>) $ sample a) mempty
          bs' = fmapCheap snd $ r'
          patches = unsafeBuildIncremental (sampleBs bs0) $
            flip pushCheap bs' $ \bs -> do
              p <- (~~) <$> sampleBs bs <*> sample (currentIncremental patches)
              return (Just (AdditivePatch p))
      in patches
    return (r0, fmapCheap fst r')
  sequenceDMapWithAdjust (dm0 :: DMap k (QueryT t q m)) dm' = do
    let loweredDm0 = mapKeyValuePairsMonotonic (\(k :=> v) -> WrapArg k :=> fmap QueryTLoweredResult ( flip runStateT [] $ unQueryT v)) dm0
        loweredDm' = fforCheap dm' $ \(PatchDMap p) -> PatchDMap $
          mapKeyValuePairsMonotonic (\(k :=> ComposeMaybe mv) -> WrapArg k :=> ComposeMaybe (fmap (fmap QueryTLoweredResult . flip runStateT [] . unQueryT) mv)) p
    (result0, result') <- QueryT $ lift $ sequenceDMapWithAdjust loweredDm0 loweredDm'
    let getValue (QueryTLoweredResult (v, _)) = v
        getWritten (QueryTLoweredResult (_, w)) = w
        liftedResult0 = mapKeyValuePairsMonotonic (\(WrapArg k :=> Identity r) -> k :=> Identity (getValue r)) result0
        liftedResult' = fforCheap result' $ \(PatchDMap p) -> PatchDMap $
          mapKeyValuePairsMonotonic (\(WrapArg k :=> ComposeMaybe mr) -> k :=> ComposeMaybe (fmap (Identity . getValue . runIdentity) mr)) p
        liftedBs0 :: Map (Some k) [Behavior t q]
        liftedBs0 = Map.fromDistinctAscList $ (\(WrapArg k :=> Identity r) -> (Some.This k, getWritten r)) <$> DMap.toList result0
        liftedBs' :: Event t (PatchMap (Some k) [Behavior t q])
        liftedBs' = fforCheap result' $ \(PatchDMap p) -> PatchMap $
          Map.fromDistinctAscList $ (\(WrapArg k :=> ComposeMaybe mr) -> (Some.This k, fmap (getWritten . runIdentity) mr)) <$> DMap.toList p
        sampleBs :: forall m'. MonadSample t m' => [Behavior t q] -> m' q
        sampleBs = foldlM (\b a -> fmap (b <>) $ sample a) mempty
    qpatch <- (\a b f -> mapAccumMaybeM_ f a b) liftedBs0 liftedBs' $ \bs0 pbs@(PatchMap bs') -> do
      patch <- fmap (AdditivePatch . fold) $ iforM bs' $ \k bs -> case Map.lookup k bs0 of
        Nothing -> case bs of
          Nothing -> return mempty
          Just newBs -> sampleBs newBs
        Just oldBs -> case bs of
          Nothing -> fmap negateG $ sampleBs oldBs
          Just newBs -> (~~) <$> sampleBs newBs <*> sampleBs oldBs
      return (apply pbs bs0, Just patch)
    tellQueryIncremental $ unsafeBuildIncremental (fmap fold $ mapM sampleBs liftedBs0) qpatch
    return (liftedResult0, liftedResult')

instance MonadTrans (QueryT t q) where
  lift = QueryT . lift . lift . lift

instance PostBuild t m => PostBuild t (QueryT t q m) where
  getPostBuild = lift getPostBuild

instance (MonadAsyncException m) => MonadAsyncException (QueryT t q m) where
  mask f = QueryT $ mask $ \unMask -> unQueryT $ f $ QueryT . unMask . unQueryT

instance TriggerEvent t m => TriggerEvent t (QueryT t q m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance (Monad m, MonadQuery t q m) => MonadQuery t q (ReaderT r m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

instance PerformEvent t m => PerformEvent t (QueryT t q m) where
  type Performable (QueryT t q m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance HasJS x m => HasJS x (QueryT t q m) where
  type JSM (QueryT t q m) = JSM m
  liftJS = lift . liftJS

instance (DomBuilder t m, MonadFix m, MonadHold t m, Group q, Query q, Additive q) => DomBuilder t (QueryT t q m) where
  type DomBuilderSpace (QueryT t q m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg (QueryT child) = QueryT $ do
    s <- get
    let cfg' = cfg
          { _elementConfig_eventSpec = _elementConfig_eventSpec cfg }
    (e, (a, newS)) <- lift $ element elementTag cfg' $ runStateT child s
    put newS
    return (e, a)

  inputElement cfg = lift $ inputElement $ cfg & inputElementConfig_elementConfig %~ liftElementConfig
  textAreaElement cfg = lift $ textAreaElement $ cfg & textAreaElementConfig_elementConfig %~ liftElementConfig
  selectElement cfg (QueryT child) = QueryT $ do
    s <- get
    let cfg' = cfg & selectElementConfig_elementConfig %~ \c ->
          c { _elementConfig_eventSpec = _elementConfig_eventSpec c }
    (e, (a, newS)) <- lift $ selectElement cfg' $ runStateT child s
    put newS
    return (e, a)
  placeRawElement = lift . placeRawElement
  wrapRawElement e cfg = lift $ wrapRawElement e $ cfg
    { _rawElementConfig_eventSpec = _rawElementConfig_eventSpec cfg
    }

instance MonadRef m => MonadRef (QueryT t q m) where
  type Ref (QueryT t q m) = Ref m
  newRef = QueryT . newRef
  readRef = QueryT . readRef
  writeRef r = QueryT . writeRef r

instance HasWebView m => HasWebView (QueryT t q m) where
  type WebViewPhantom (QueryT t q m) = WebViewPhantom m
  askWebView = QueryT askWebView

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (QueryT t q m) where
  newEventWithTrigger = QueryT . newEventWithTrigger
  newFanEventWithTrigger a = QueryT . lift $ newFanEventWithTrigger a

mapQuery :: QueryMorphism q q' -> q -> q'
mapQuery = _queryMorphism_mapQuery

mapQueryResult :: QueryMorphism q q' -> QueryResult q' -> QueryResult q
mapQueryResult = _queryMorphism_mapQueryResult

withQueryT :: (MonadFix m, PostBuild t m, Group q, Group q', Additive q, Additive q', Query q')
           => QueryMorphism q q'
           -> QueryT t q m a
           -> QueryT t q' m a
withQueryT f a = do
  r' <- askQueryResult
  (result, q) <- lift $ runQueryT a $ mapQueryResult f <$> r'
  tellQueryIncremental $ unsafeBuildIncremental
    (fmap (mapQuery f) (sample (currentIncremental q)))
    (fmapCheap (AdditivePatch . mapQuery f . unAdditivePatch) $ updatedIncremental q)
  return result

dynWithQueryT :: (MonadFix m, PostBuild t m, Group q, Additive q, Group q', Additive q', Query q')
           => Dynamic t (QueryMorphism q q')
           -> QueryT t q m a
           -> QueryT t q' m a
dynWithQueryT f q = do
  r' <- askQueryResult
  (result, q') <- lift $ runQueryT q $ zipDynWith mapQueryResult f r'
  tellQueryIncremental $ zipDynIncrementalWith mapQuery f q'
  return result
 where zipDynIncrementalWith g da ib =
         let eab = align (updated da) (updatedIncremental ib)
             ec = flip push eab $ \o -> case o of
                 This a -> do
                   aOld <- sample $ current da
                   b <- sample $ currentIncremental ib
                   return $ Just $ AdditivePatch (g a b ~~ g aOld b)
                 That (AdditivePatch b) -> do
                   a <- sample $ current da
                   return $ Just $ AdditivePatch $ (g a b)
                 These a (AdditivePatch b) -> do
                   aOld <- sample $ current da
                   bOld <- sample $ currentIncremental ib
                   return $ Just $ AdditivePatch $ mconcat [ g a bOld, negateG (g aOld bOld), g a b]
         in unsafeBuildIncremental (g <$> sample (current da) <*> sample (currentIncremental ib)) ec

type FocusWidgetInternal app t m = QueryT t (ViewSelector app SelectedCount) (RequesterT t (AppRequest app) Identity m)

newtype FocusWidget app t m a = FocusWidget { unFocusWidget :: FocusWidgetInternal app t m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadException)

instance MonadTrans (FocusWidget app t) where
  lift = FocusWidget . lift . lift

instance HasJS x m => HasJS x (FocusWidget app t m) where
  type JSM (FocusWidget app t m) = JSM m
  liftJS = lift . liftJS

instance (HasEnv app, MonadWidget' t m, PrimMonad m) => Requester t (FocusWidget app t m) where
  type Request (FocusWidget app t m) = AppRequest app
  type Response (FocusWidget app t m) = Identity
  withRequesting f = FocusWidget $ withRequesting $ unFocusWidget . f

instance PerformEvent t m => PerformEvent t (FocusWidget app t m) where
  type Performable (FocusWidget app t m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance TriggerEvent t m => TriggerEvent t (FocusWidget app t m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance (HasView app, DomBuilder t m, MonadHold t m, Ref (Performable m) ~ Ref m, MonadFix m, Group (ViewSelector app SelectedCount), Additive (ViewSelector app SelectedCount)) => DomBuilder t (FocusWidget app t m) where
  type DomBuilderSpace (FocusWidget app t m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg (FocusWidget child) = FocusWidget $ element elementTag (fmap1 unFocusWidget cfg) child
  inputElement cfg = FocusWidget $ inputElement $ fmap1 unFocusWidget cfg
  textAreaElement cfg = FocusWidget $ textAreaElement $ fmap1 unFocusWidget cfg
  selectElement cfg (FocusWidget child) = FocusWidget $ selectElement (fmap1 unFocusWidget cfg) child
  placeRawElement = FocusWidget . placeRawElement
  wrapRawElement e cfg = FocusWidget $ wrapRawElement e $ fmap1 unFocusWidget cfg

instance (Reflex t, MonadFix m, MonadHold t m, MonadAdjust t m, Group (ViewSelector app SelectedCount), Additive (ViewSelector app SelectedCount), Query (ViewSelector app SelectedCount)) => MonadAdjust t (FocusWidget app t m) where
  runWithReplace a0 a' = FocusWidget $ runWithReplace (coerce a0) (coerceEvent a')
  sequenceDMapWithAdjust dm0 dm' = FocusWidget $ sequenceDMapWithAdjust (coerce dm0) (coerceEvent dm')

instance PostBuild t m => PostBuild t (FocusWidget app t m) where
  getPostBuild = lift getPostBuild

instance MonadRef m => MonadRef (FocusWidget app t m) where
  type Ref (FocusWidget app t m) = Ref m
  newRef = FocusWidget . newRef
  readRef = FocusWidget . readRef
  writeRef r = FocusWidget . writeRef r

instance MonadHold t m => MonadHold t (FocusWidget app t m) where
  hold a = FocusWidget . hold a
  holdDyn a = FocusWidget . holdDyn a
  holdIncremental a = FocusWidget . holdIncremental a

instance MonadSample t m => MonadSample t (FocusWidget app t m) where
  sample = FocusWidget . sample

instance HasWebView m => HasWebView (FocusWidget app t m) where
  type WebViewPhantom (FocusWidget app t m) = WebViewPhantom m
  askWebView = FocusWidget askWebView

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (FocusWidget app t m) where
  newEventWithTrigger = FocusWidget . newEventWithTrigger
  newFanEventWithTrigger a = FocusWidget . lift $ newFanEventWithTrigger a

instance Requester t m => R.Requester t (QueryT t q m) where
  type Request (QueryT t q m) = R.Request m
  type Response (QueryT t q m) = R.Response m
  withRequesting f = QueryT $ withRequesting $ unQueryT . f

-- | This synonym adds constraints to MonadFocusWidget that are only available on the frontend, and not via backend rendering.
type MonadFocusFrontendWidget app t m =
    ( MonadFocusWidget app t m
    , DomBuilderSpace m ~ GhcjsDomSpace
    )

class (HasView app) => HasEnv app where
  data Env app :: * -> *
  getToken :: Env app t -> Dynamic t (Maybe (Signed AuthToken)) -- This is a Maybe to handle logged-out interactions
  getViews :: Env app t -> Dynamic t (Map (Signed AuthToken) (View app))

class (HasRequest app, HasView app, Group (ViewSelector app SelectedCount), Additive (ViewSelector app SelectedCount)) => HasFocus app

class ( MonadWidget' t m
      , MonadFix (WidgetHost m)
      , Requester t m
      , R.Request m ~ AppRequest app
      , Response m ~ Identity
      , HasFocus app
      , MonadQuery t (ViewSelector app SelectedCount) m
      ) => MonadFocusWidget app t m | m -> app t where

instance ( MonadWidget' t m
         , MonadFix (WidgetHost m)
         , Requester t m
         , R.Request m ~ AppRequest app
         , Response m ~ Identity
         , HasFocus app
         , MonadQuery t (ViewSelector app SelectedCount) m
         ) => MonadFocusWidget app t m

watchViewSelector :: (MonadFocusWidget app t m) => Dynamic t (ViewSelector app SelectedCount) -> m (Dynamic t (View app))
watchViewSelector = fmap uniqDyn . queryDyn

--TODO: HasDocument is still not accounted for
type MonadWidget' t m =
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadSample t (Performable m)
  , MonadReflexCreateTrigger t m
  , PostBuild t m
  , PerformEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  , TriggerEvent t m
  -- , HasWebView m
  -- , HasWebView (Performable m)
  -- , MonadAsyncException m
  -- , MonadAsyncException (Performable m)
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  )

runFocusWidget :: forall t m a x app. ( MonadWidget' t m
                                      , HasWebView m
                                      , HasJS x m
                                      , HasFocus app
                                      , Eq (ViewSelector app SelectedCount)
                                      )
               => Signed AuthToken
               -> FocusWidget app t m a
               -> m a
runFocusWidget = runFocusWidget' (Right "/listen")

runFocusWidget' :: forall t m a x app. ( MonadWidget' t m
                                       , HasWebView m
                                       , HasJS x m
                                       , HasFocus app
                                       , Eq (ViewSelector app SelectedCount)
                                       )
                => Either WebSocketUrl Text
                -> Signed AuthToken
                -> FocusWidget app t m a
                -> m a
runFocusWidget' murl token child = do
  pb <- getPostBuild
  rec (notification, response) <- openWebSocket murl request' updatedVS
      (request', response') <- identifyTags request response
      ((a, vs), request) <- flip runRequesterT response' $ runQueryT (withQueryT (singletonQuery token) (unFocusWidget child)) e
      let nubbedVs = uniqDyn (incrementalToDynamic vs)
          updatedVS = leftmost [updated nubbedVs, tag (current nubbedVs) pb]
      e :: Dynamic t (AppendMap (Signed AuthToken) (QueryResult (ViewSelector app SelectedCount))) <- fromNotifications nubbedVs notification
  return a

fromNotifications :: forall m t k vs. (Query vs, MonadHold t m, Reflex t, MonadFix m, Ord k)
                  => Dynamic t (AppendMap k vs)
                  -> Event t (AppendMap k (QueryResult vs))
                  -> m (Dynamic t (AppendMap k (QueryResult vs)))
fromNotifications vs ePatch = do
  views <- foldDyn (\(vs', p) v -> applyAndCrop p vs' v) AppendMap.empty $ attach (current vs) ePatch
  return views
  where
    applyPatch' m0 m1 = AppendMap.mergeWithKey (\_ x y -> Just (x <> y)) id (const AppendMap.empty) m0 m1
    cropView' m0 m1 = AppendMap.mergeWithKey (\_ x y -> Just (cropView x y)) (fmap (\_ -> mempty)) (const AppendMap.empty) m0 m1
    applyAndCrop p vs' v = cropView' vs' $ applyPatch' p v

data Decoder f = forall a. FromJSON a => Decoder (f a)

identifyTags :: forall t k v m. (MonadFix m, MonadHold t m, Reflex t, Request v) => Event t (DMap k v) -> Event t (Data.Aeson.Value, Either Text Data.Aeson.Value) -> m (Event t [(Data.Aeson.Value, Data.Aeson.Value)], Event t (DMap k Identity))
identifyTags send recv = do
  rec nextId :: Behavior t Int <- hold 1 $ fmap (\(a, _, _) -> a) send'
      waitingFor :: Incremental t (PatchMap Int (Decoder k)) <- holdIncremental mempty $ leftmost
        [ fmap (\(_, b, _) -> b) send'
        , fmap snd recv'
        ]
      let send' = flip pushAlways send $ \dm -> do
            oldNextId <- sample nextId
            let (result, newNextId) = flip runState oldNextId $ forM (DMap.toList dm) $ \(k :=> v) -> do
                  n <- get
                  put $ succ n
                  return (n, k :=> v)
                patchWaitingFor = PatchMap $ Map.fromList $ ffor result $ \(n, k :=> v) -> case requestResponseFromJSON v of
                  Dict -> (n, Just (Decoder k))
                toSend = ffor result $ \(n, _ :=> v) -> (toJSON n, requestToJSON v)
            return (newNextId, patchWaitingFor, toSend)
      let recv' = flip push recv $ \(jsonN, jsonV') -> do
            case jsonV' of
              Left _ -> return Nothing
              Right jsonV -> do
                wf <- sample $ currentIncremental waitingFor
                case parseMaybe parseJSON jsonN of
                  Nothing -> return Nothing
                  Just n ->
                    return $ case Map.lookup n wf of
                      Just (Decoder k) -> Just $
                        let Just v = parseMaybe parseJSON jsonV
                        in (DMap.singleton k $ Identity v, PatchMap $ Map.singleton n Nothing)
                      Nothing -> Nothing
  return (fmap (\(_, _, c) -> c) send', fst <$> recv')

-- | Open a websocket connection and split resulting incoming traffic into listen notification and api response channels
openWebSocket' :: forall t x m vs v.
                 ( MonadIO m
                 , MonadIO (Performable m)
                 , PostBuild t m
                 , TriggerEvent t m
                 , PerformEvent t m
                 , HasWebView m
                 , HasJS x m
                 , MonadFix m
                 , FromJSON v
                 , ToJSON vs
                 )
              => Either WebSocketUrl Text -- ^ Either a complete URL or just a path (the websocket code will try to infer the protocol and hostname)
              -> Event t [(Data.Aeson.Value, Data.Aeson.Value)] -- ^ Outbound requests
              -> Dynamic t vs -- ^ Authenticated listen requests (e.g., ViewSelector updates)
              -> m ( Event t v
                   , Event t (Data.Aeson.Value, Either Text Data.Aeson.Value)
                   , Event t Text
                   )
openWebSocket' murl request vs = do
#ifdef ghcjs_HOST_OS
  rec let platformDecode = rawDecode
      ws <- rawWebSocket murl $ def
#else
  rec let platformDecode = decodeValue' . LBS.fromStrict
      ws <- webSocket murl $ def
#endif
        & webSocketConfig_send .~ fmap (map (decodeUtf8 . LBS.toStrict . encode)) (mconcat
          [ fmap (map (uncurry WebSocketData_Api)) request
          , fmap ((:[]) . WebSocketData_Listen) $ updated vs
          , tag (fmap ((:[]) . WebSocketData_Listen) $ current vs) $ _webSocket_open ws
          ])
  let (eMessages :: Event t (Either Text (WebSocketData v (Either Text Data.Aeson.Value)))) = fmapMaybe platformDecode $ _webSocket_recv ws
  --TODO: Handle parse errors returned by the backend
      notification = fmapMaybe (^? _Right . _WebSocketData_Listen) eMessages
      response = fmapMaybe (^? _Right . _WebSocketData_Api) eMessages
      version = fmapMaybe (^? _Right . _WebSocketData_Version) eMessages
  return (notification, response, version)

openWebSocket :: forall t x m vs v.
                 ( MonadIO m
                 , MonadIO (Performable m)
                 , PostBuild t m
                 , TriggerEvent t m
                 , PerformEvent t m
                 , HasWebView m
                 , HasJS x m
                 , MonadFix m
                 , MonadHold t m
                 , FromJSON v
                 , ToJSON vs
                 , Monoid vs
                 )
              => Either WebSocketUrl Text -- ^ Either a complete URL or just a path (the websocket code will try to infer the protocol and hostname)
              -> Event t [(Data.Aeson.Value, Data.Aeson.Value)] -- ^ Outbound requests
              -> Event t vs -- ^ Authenticated listen requests (e.g., ViewSelector updates)
              -> m ( Event t v
                   , Event t (Data.Aeson.Value, Either Text Data.Aeson.Value)
                   )
openWebSocket murl request updatedVs = do
  vs <- holdDyn mempty updatedVs
  (f, s, _) <- openWebSocket' murl request vs
  return (f, s)
