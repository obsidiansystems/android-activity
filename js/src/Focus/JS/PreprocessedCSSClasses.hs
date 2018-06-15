{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Focus.JS.PreprocessedCSSClasses where

import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Text as T

import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Coerce
import qualified Data.Map as Map
import Foreign.JavaScript.TH
import Reflex
import Reflex.Dom.Builder.Class
import Reflex.Host.Class

import Unsafe.Coerce

-- | A DomBuilder transformer that removes the prefix "preprocess-" from all css classes
newtype PreprocessedCSSClassesT m a = PreprocessedCSSClassesT { runPreprocessedCSSClassesT :: m a } deriving (Functor, Applicative, Monad, MonadAtomicRef, MonadFix, MonadIO)

deriving instance MonadSample t m => MonadSample t (PreprocessedCSSClassesT m)
deriving instance MonadHold t m => MonadHold t (PreprocessedCSSClassesT m)

instance MonadTrans PreprocessedCSSClassesT where
  lift = PreprocessedCSSClassesT

instance MonadTransControl PreprocessedCSSClassesT where
  type StT PreprocessedCSSClassesT a = a
  liftWith f = PreprocessedCSSClassesT $ f runPreprocessedCSSClassesT
  restoreT = PreprocessedCSSClassesT

instance MonadRef m => MonadRef (PreprocessedCSSClassesT m) where
  type Ref (PreprocessedCSSClassesT m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref

instance PerformEvent t m => PerformEvent t (PreprocessedCSSClassesT m) where
  type Performable (PreprocessedCSSClassesT m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance PrimMonad m => PrimMonad (PreprocessedCSSClassesT m) where
  type PrimState (PreprocessedCSSClassesT m) = PrimState m
  primitive = lift . primitive

processCssElementConfig :: Reflex t => ElementConfig er t m -> ElementConfig er t m
processCssElementConfig cfg = cfg
  { _elementConfig_initialAttributes = Map.adjust (T.unwords . map removePrefix . T.words) "class" $ _elementConfig_initialAttributes cfg
  , _elementConfig_modifyAttributes = fmap (Map.adjust (fmap (T.unwords . map addPrefix . T.words)) "class") <$> _elementConfig_modifyAttributes cfg
  }
  where
    classPrefix = "preprocess-"
    removePrefix s = fromMaybe s (T.stripPrefix classPrefix s)
    addPrefix s = if s `elem` transformedClasses then T.append classPrefix s else s
    transformedClasses = mapMaybe (T.stripPrefix classPrefix) $ maybe [] T.words (Map.lookup "class" (_elementConfig_initialAttributes cfg))

instance PostBuild t m => PostBuild t (PreprocessedCSSClassesT m) where
  getPostBuild = lift getPostBuild

deriving instance TriggerEvent t m => TriggerEvent t (PreprocessedCSSClassesT m)

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (PreprocessedCSSClassesT m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance Adjustable t m => Adjustable t (PreprocessedCSSClassesT m) where
  traverseDMapWithKeyWithAdjust f dm0 dm' = PreprocessedCSSClassesT $ traverseDMapWithKeyWithAdjust (\k v -> runPreprocessedCSSClassesT $ f k v) (coerce dm0) (unsafeCoerce dm') --TODO: Eliminate unsafeCoerce
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = PreprocessedCSSClassesT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> runPreprocessedCSSClassesT $ f k v) (coerce dm0) (unsafeCoerce dm') --TODO: Eliminate unsafeCoerce
  runWithReplace a0 a' = lift $ runWithReplace (runPreprocessedCSSClassesT a0) (runPreprocessedCSSClassesT <$> a')


instance NotReady t m => NotReady t (PreprocessedCSSClassesT m) where
    notReadyUntil = lift . notReadyUntil
    notReady = lift notReady

instance DomBuilder t m => DomBuilder t (PreprocessedCSSClassesT m) where
  type DomBuilderSpace (PreprocessedCSSClassesT m) = DomBuilderSpace m
  element elementTag cfg child = do
    lift $ element elementTag cfg $ runPreprocessedCSSClassesT child

instance HasJSContext m => HasJSContext (PreprocessedCSSClassesT m) where
  type JSContextPhantom (PreprocessedCSSClassesT m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance HasJS js m => HasJS js (PreprocessedCSSClassesT m) where
  type JSX (PreprocessedCSSClassesT m) = JSX m
  liftJS = lift . liftJS
