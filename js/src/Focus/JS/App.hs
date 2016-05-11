{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeFamilies, UndecidableInstances, FunctionalDependencies, RankNTypes, RecursiveDo, ScopedTypeVariables #-}
module Focus.JS.App where

import Control.Lens hiding (ix, views)
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Aeson
import Data.Map (Map)
import Data.Semigroup
import Focus.Account
import Focus.Api
import Focus.App
import Focus.AppendMap
import Focus.JS.Request
import Focus.Request
import Focus.Sign
import Reflex.Dom
import Reflex.Host.Class
import qualified Data.Map as Map
import qualified Data.Set as Set

type FocusWidgetInternal app t m = RequestT t (AppRequest app) (DynamicWriterT t (AppendMap (Signed AuthToken) (ViewSelector app)) (ReaderT (Env app t) m))

newtype FocusWidget app t m a = FocusWidget { unFocusWidget :: FocusWidgetInternal app t m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadException, MonadAsyncException)

deriving instance (HasEnv app, MonadFix (WidgetHost m), MonadWidget t m, Semigroup (ViewSelector app), Request (PublicRequest app), Request (PrivateRequest app)) => MonadRequest t (AppRequest app) (FocusWidget app t m)

instance (HasEnv app, MonadWidget t m, Semigroup (ViewSelector app)) => MonadWidget t (FocusWidget app t m) where
  type WidgetHost (FocusWidget app t m) = WidgetHost (FocusWidgetInternal app t m)
  type GuiAction (FocusWidget app t m) = GuiAction (FocusWidgetInternal app t m)
  type WidgetOutput t (FocusWidget app t m) = WidgetOutput t (FocusWidgetInternal app t m)
  askParent = FocusWidget askParent
  subWidget n w = FocusWidget $ subWidget n $ unFocusWidget w
  subWidgetWithVoidActions n w = FocusWidget $ subWidgetWithVoidActions n $ unFocusWidget w
  liftWidgetHost = FocusWidget . liftWidgetHost
  schedulePostBuild = FocusWidget . schedulePostBuild
  addVoidAction = FocusWidget . addVoidAction
  getRunWidget = FocusWidget $ do
    runWidget' <- getRunWidget
    return $ \rootElement w -> runWidget' rootElement $ unFocusWidget w
  tellWidgetOutput = FocusWidget . tellWidgetOutput

instance MonadRef m => MonadRef (FocusWidget app t m) where
  type Ref (FocusWidget app t m) = Ref m
  newRef = FocusWidget . newRef
  readRef = FocusWidget . readRef
  writeRef r = FocusWidget . writeRef r

instance MonadHold t m => MonadHold t (FocusWidget app t m) where
  hold a = FocusWidget . hold a

instance MonadSample t m => MonadSample t (FocusWidget app t m) where
  sample = FocusWidget . sample

instance HasWebView m => HasWebView (FocusWidget app t m) where
  type WebViewPhantom (FocusWidget app t m) = WebViewPhantom m
  askWebView = FocusWidget askWebView

instance HasPostGui t h m => HasPostGui t h (FocusWidget app t m) where
  askPostGui = FocusWidget askPostGui
  askRunWithActions = FocusWidget askRunWithActions
  scheduleFollowup r a = FocusWidget $ lift $ scheduleFollowup r a

instance HasDocument m => HasDocument (FocusWidget app t m) where
  askDocument = FocusWidget askDocument

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (FocusWidget app t m) where
  newEventWithTrigger = FocusWidget . newEventWithTrigger
  newFanEventWithTrigger a = FocusWidget . lift $ newFanEventWithTrigger a

class ( MonadDynamicWriter t (AppendMap (Signed AuthToken) (ViewSelector app)) m
      , MonadWidget t m
      , MonadFix (WidgetHost m)
      , MonadRequest t (AppRequest app) m
      , HasFocus app
      ) => MonadFocusWidget app t m | m -> app where
  askEnv :: m (Env app t)
  tellInterest :: Dynamic t (ViewSelector app) -> m ()
  getView :: m (Dynamic t (View app))

class (HasView app) => HasEnv app where
  data Env app :: * -> *
  getToken :: Env app t -> Dynamic t (Maybe (Signed AuthToken)) -- This is a Maybe to handle logged-out interactions
  getViews :: Env app t -> Dynamic t (Map (Signed AuthToken) (View app))

class (HasEnv app, HasRequest app, HasView app) => HasFocus app

instance (HasFocus app, MonadFix (WidgetHost m), MonadWidget t m) => MonadFocusWidget app t (FocusWidget app t m) where
  askEnv = FocusWidget $ lift ask
  tellInterest is = do
    token <- asksEnv getToken
    tellDyn =<< combineDyn (\mt is' -> maybe mempty (\t -> AppendMap (Map.singleton t is')) mt) token is
  getView = do
    token <- asksEnv getToken
    views <- asksEnv getViews
    combineDyn (maybe (const emptyView) (\t -> maybe emptyView id . Map.lookup t)) token views

instance (HasEnv app, Monad m) => MonadDynamicWriter t (AppendMap (Signed AuthToken) (ViewSelector app)) (FocusWidget app t m) where
  tellDyn = FocusWidget . lift . tellDyn

watchViewSelector :: MonadFocusWidget app t m => Dynamic t (ViewSelector app) -> m (Dynamic t (View app))
watchViewSelector s = do
  tellInterest s
  combineDyn cropView s =<< getView

watchViewSelectorLens :: (Monoid a, MonadFocusWidget app t m) => ASetter a (ViewSelector app) c b -> Dynamic t b -> m (Dynamic t (View app))
watchViewSelectorLens l sdyn = do
  s <- mapDyn (\s' -> mempty & l .~ s') sdyn
  tellInterest s
  combineDyn cropView s =<< getView

watchViewSelectorLensSet :: (Monoid a, MonadFocusWidget app t m) => ASetter a (ViewSelector app) b (Set.Set c) -> Dynamic t c -> m (Dynamic t (View app))
watchViewSelectorLensSet l sdyn = watchViewSelectorLens l =<< mapDyn Set.singleton sdyn

asksEnv :: MonadFocusWidget app t m => (Env app t -> a) -> m a
asksEnv f = fmap f askEnv

asksView :: MonadFocusWidget app t m => ((View app) -> a) -> m (Dynamic t a)
asksView f = mapDyn f =<< getView

runFocusWidget :: forall t m a x app. ( MonadWidget t m
                                      , HasJS x m
                                      , HasJS x (WidgetHost m)
                                      , HasFocus app
                                      , ToJSON (ViewSelector app)
                                      , ToJSON (ViewPatch app)
                                      , FromJSON (ViewPatch app)
                                      , Eq (ViewSelector app)
                                      )
               => Dynamic t (Maybe (Signed AuthToken))
               -> (Dynamic t (Maybe (Signed AuthToken)) -> Dynamic t (Map (Signed AuthToken) (View app)) -> Env app t)
               -> FocusWidget app t m a
               -> m (a, Dynamic t (AppendMap (Signed AuthToken) (ViewSelector app)))
runFocusWidget tokenDyn mkEnv child = do
  let child' = do
        is0 <- forDyn tokenDyn $ maybe mempty (\t -> AppendMap (Map.singleton t mempty))
        tellDyn is0
        child
  rec ((a, patches), vs) <- runReaderT (runDynamicWriterT (runRequestT (updated nubbedVs) (unFocusWidget child'))) e
      let nubbedVs = nubDyn vs
      e <- fromNotifications nubbedVs patches
  return (a, vs)
  where
    applyPatch' :: Ord k => AppendMap k (ViewPatch app) -> Map k (View app) -> Map k (View app)
    applyPatch' (AppendMap m0) m1 = Map.mergeWithKey (\_ x y -> Just (patchView x y)) (fmap (\p -> patchView p emptyView)) (const Map.empty) m0 m1
    cropView' :: Ord k => AppendMap k (ViewSelector app) -> Map k (View app) -> Map k (View app)
    cropView' (AppendMap m0) m1 = Map.mergeWithKey (\_ x y -> Just (cropView x y)) (fmap (\c -> cropView c emptyView)) (const Map.empty) m0 m1
    applyAndCrop :: Ord k => AppendMap k (ViewPatch app) -> AppendMap k (ViewSelector app) -> Map k (View app) -> Map k (View app)
    applyAndCrop p vs v = cropView' vs $ applyPatch' p v
    fromNotifications vs (ePatch :: Event t (AppendMap (Signed AuthToken) (ViewPatch app))) = do
      views <- foldDyn (\(vs', p) v -> applyAndCrop p vs' v) Map.empty $ attachDyn vs ePatch
      return $ mkEnv tokenDyn views
