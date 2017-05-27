{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Focus.JS.App where

import Control.Lens ( (^?), _Right)
import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.Constraint
import Data.Dependent.Map (DMap, DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
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
import Reflex.Dom.Core hiding (MonadWidget, webSocket, Request)
import Reflex.Host.Class
#ifndef ghcjs_HOST_OS
import GHCJS.DOM.Types (MonadJSM(..))
#else
import GHCJS.DOM.Types (MonadJSM)
#endif

type FocusWidgetInternal f app t m = QueryT t (ViewSelector app SelectedCount) (RequesterT t (AppRequest f app) Identity m)

newtype FocusWidget f app t m a = FocusWidget { unFocusWidget :: FocusWidgetInternal f app t m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadException)

#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (FocusWidget f app t m) where
  liftJSM' = lift . liftJSM'
#endif

instance MonadTrans (FocusWidget f app t) where
  lift = FocusWidget . lift . lift

instance HasJS x m => HasJS x (FocusWidget f app t m) where
  type JSX (FocusWidget f app t m) = JSX m
  liftJS = lift . liftJS

instance (HasEnv f app, MonadWidget' t m, PrimMonad m) => Requester t (FocusWidget f app t m) where
  type Request (FocusWidget f app t m) = AppRequest f app
  type Response (FocusWidget f app t m) = Identity
  withRequesting f = FocusWidget $ withRequesting $ unFocusWidget . f

instance PerformEvent t m => PerformEvent t (FocusWidget f app t m) where
  type Performable (FocusWidget f app t m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance TriggerEvent t m => TriggerEvent t (FocusWidget f app t m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance (HasView app, DomBuilder t m, MonadHold t m, Ref (Performable m) ~ Ref m, MonadFix m, Group (ViewSelector app SelectedCount), Additive (ViewSelector app SelectedCount)) => DomBuilder t (FocusWidget f app t m) where
  type DomBuilderSpace (FocusWidget f app t m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg (FocusWidget child) = FocusWidget $ element elementTag cfg child
  inputElement = FocusWidget . inputElement
  textAreaElement = FocusWidget . textAreaElement
  selectElement cfg (FocusWidget child) = FocusWidget $ selectElement cfg child
  placeRawElement = FocusWidget . placeRawElement
  wrapRawElement e = FocusWidget . wrapRawElement e

instance (Reflex t, MonadFix m, MonadHold t m, MonadAdjust t m, Group (ViewSelector app SelectedCount), Additive (ViewSelector app SelectedCount), Query (ViewSelector app SelectedCount)) => MonadAdjust t (FocusWidget f app t m) where
  runWithReplace a0 a' = FocusWidget $ runWithReplace (coerce a0) (coerceEvent a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = FocusWidget $ traverseDMapWithKeyWithAdjust (\k v -> unFocusWidget $ f k v) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = FocusWidget $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unFocusWidget $ f k v) (coerce dm0) (coerceEvent dm')

instance PostBuild t m => PostBuild t (FocusWidget f app t m) where
  getPostBuild = lift getPostBuild

instance MonadRef m => MonadRef (FocusWidget f app t m) where
  type Ref (FocusWidget f app t m) = Ref m
  newRef = FocusWidget . newRef
  readRef = FocusWidget . readRef
  writeRef r = FocusWidget . writeRef r

instance MonadHold t m => MonadHold t (FocusWidget f app t m) where
  hold a = FocusWidget . hold a
  holdDyn a = FocusWidget . holdDyn a
  holdIncremental a = FocusWidget . holdIncremental a

instance MonadSample t m => MonadSample t (FocusWidget f app t m) where
  sample = FocusWidget . sample

instance HasJSContext m => HasJSContext (FocusWidget f app t m) where
  type JSContextPhantom (FocusWidget f app t m) = JSContextPhantom m
  askJSContext = FocusWidget askJSContext

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (FocusWidget f app t m) where
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

class (HasView app) => HasEnv f app where
  data Env app :: * -> *
  getToken :: Env app t -> Dynamic t (Maybe (Signed (AuthToken f))) -- This is a Maybe to handle logged-out interactions
  getViews :: Env app t -> Dynamic t (Map (Signed (AuthToken f)) (View app))

class (HasRequest app f, HasView app, Group (ViewSelector app SelectedCount), Additive (ViewSelector app SelectedCount)) => HasFocus f app

class ( MonadWidget' t m
      , MonadFix (WidgetHost m)
      , Requester t m
      , R.Request m ~ AppRequest Identity app
      , Response m ~ Identity
      , HasFocus Identity app
      , MonadQuery t (ViewSelector app SelectedCount) m
      ) => MonadFocusWidget app t m | m -> app t where

instance ( MonadWidget' t m
         , MonadFix (WidgetHost m)
         , Requester t m
         , R.Request m ~ AppRequest Identity app
         , Response m ~ Identity
         , HasFocus Identity app
         , MonadQuery t (ViewSelector app SelectedCount) m
         ) => MonadFocusWidget app t m

queryDynUniq :: ( Monad m
                , Reflex t
                , MonadQuery t q m
                , Eq (QueryResult q)
                )
             => Dynamic t q
             -> m (Dynamic t (QueryResult q))
queryDynUniq = fmap uniqDyn . queryDyn

watchViewSelector :: ( Monad m
                     , Reflex t
                     , MonadQuery t q m
                     , Eq (QueryResult q)
                     )
                  => Dynamic t q
                  -> m (Dynamic t (QueryResult q))
watchViewSelector = queryDynUniq

--TODO: HasDocument is still not accounted for
type MonadWidget' t m =
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadSample t (Performable m)
  , MonadReflexCreateTrigger t m
  , PostBuild t m
  , PerformEvent t m
  , TriggerEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  -- , MonadJSM m
  -- , MonadJSM (Performable m)
  -- , HasJSContext m
  -- , HasJSContext (Performable m)
  -- , MonadAsyncException m
  -- , MonadAsyncException (Performable m)
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  )

runFocusWidget :: forall t m a x f app.
                ( MonadWidget' t m
                , HasJSContext m
                , HasJS x m
                , MonadJSM m
                , MonadJSM (Performable m)
                , HasFocus f app
                , Eq (ViewSelector app SelectedCount)
                )
               => Signed (AuthToken f)
               -> FocusWidget f app t m a
               -> m a
runFocusWidget = runFocusWidget' (Right "/listen")

runFocusWidget' :: forall t m a x f app.
                 ( MonadWidget' t m
                 , HasJSContext m
                 , HasJS x m
                 , MonadJSM m
                 , MonadJSM (Performable m)
                 , HasFocus f app
                 , Eq (ViewSelector app SelectedCount)
                 )
                => Either WebSocketUrl Text
                -> Signed (AuthToken f)
                -> FocusWidget f app t m a
                -> m a
runFocusWidget' murl token child = do
  pb <- getPostBuild
  rec (notification, response) <- openWebSocket murl request' updatedVS
      (request', response') <- identifyTags request response
      ((a, vs), request) <- flip runRequesterT response' $ runQueryT (withQueryT (singletonQuery token) (unFocusWidget child)) e
      let nubbedVs = uniqDyn (incrementalToDynamic vs)
          updatedVS = leftmost [updated nubbedVs, tag (current nubbedVs) pb]
      e :: Dynamic t (AppendMap (Signed (AuthToken f)) (QueryResult (ViewSelector app SelectedCount))) <- fromNotifications nubbedVs notification
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

data AppWebSocket t v = AppWebSocket
  { _appWebSocket_notification :: Event t v
  , _appWebSocket_response :: Event t (Data.Aeson.Value, Either Text Data.Aeson.Value)
  , _appWebSocket_version :: Event t Text
  , _appWebSocket_connected :: Dynamic t Bool
  }

-- | Open a websocket connection and split resulting incoming traffic into listen notification and api response channels
openWebSocket' :: forall t x m vs v.
                 ( MonadJSM m
                 , MonadJSM (Performable m)
                 , PostBuild t m
                 , TriggerEvent t m
                 , PerformEvent t m
                 , HasJSContext m
                 , HasJS x m
                 , MonadFix m
                 , MonadHold t m
                 , FromJSON v
                 , ToJSON vs
                 )
              => Either WebSocketUrl Text -- ^ Either a complete URL or just a path (the websocket code will try to infer the protocol and hostname)
              -> Event t [(Data.Aeson.Value, Data.Aeson.Value)] -- ^ Outbound requests
              -> Dynamic t vs -- ^ Authenticated listen requests (e.g., ViewSelector updates)
              -> m (AppWebSocket t v)
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
  connected <- holdDyn False . leftmost $ [True <$ _webSocket_open ws, False <$ _webSocket_close ws]
  return $ AppWebSocket
    { _appWebSocket_notification = notification
    , _appWebSocket_response = response
    , _appWebSocket_version = version
    , _appWebSocket_connected = connected
    }

openWebSocket :: forall t x m vs v.
                 ( MonadJSM m
                 , MonadJSM (Performable m)
                 , PostBuild t m
                 , TriggerEvent t m
                 , PerformEvent t m
                 , HasJSContext m
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
  aws <- openWebSocket' murl request vs
  return (_appWebSocket_notification aws, _appWebSocket_response aws)
