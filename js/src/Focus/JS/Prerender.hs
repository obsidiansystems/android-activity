{-# LANGUAGE ScopedTypeVariables, TypeApplications, ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, Rank2Types #-}
module Focus.JS.Prerender
       ( Prerender (..)
       , prerender
       ) where

import Focus.JS.App
import Focus.JS.Request

import Control.Monad.Reader
import Data.Constraint
import Reflex.Dom

type PrerenderClientConstraint js m = (HasJS js m, HasJS js (Performable m), MonadFix m, MonadFix (Performable m))

class Prerender js m | m -> js where
  prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m))

-- | Draw one widget when prerendering (e.g. server-side) and another when the
-- widget is fully instantiated.  In a given execution of this function, there
-- will be exactly one invocation of exactly one of the arguments.
prerender :: forall js m a. Prerender js m => m a -> (PrerenderClientConstraint js m => m a) -> m a
prerender server client = case prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)) of
  Nothing -> server
  Just Dict -> client

instance (HasJS js (Widget x), HasJS js (Performable (Widget x))) => Prerender js (Widget x) where
  prerenderClientDict = Just Dict

instance Prerender js m => Prerender js (FocusWidget env t m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict @js @m)

instance Prerender js m => Prerender js (DynamicWriterT t w m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict @js @m)

instance Prerender js m => Prerender js (ReaderT w m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict @js @m)

instance Prerender js m => Prerender js (RequestT t req m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict @js @m)
