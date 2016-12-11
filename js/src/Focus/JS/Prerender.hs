{-# LANGUAGE ScopedTypeVariables, TypeApplications, ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, Rank2Types, TypeFamilies, EmptyDataDecls #-}
module Focus.JS.Prerender
       ( Prerender (..)
       , prerender
       ) where

import Focus.JS.App

import Control.Monad.Reader
import Data.Constraint
import Reflex.Host.Class
import Reflex.Dom

import Focus.JS.PreprocessedCSSClasses

type PrerenderClientConstraint js m =
  ( HasJS js m
  , HasJS js (Performable m)
  , HasWebView m
  , HasWebView (Performable m)
  , MonadFix m
  , MonadFix (Performable m)
  )

class Prerender js m | m -> js where
  prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m))

-- | Draw one widget when prerendering (e.g. server-side) and another when the
-- widget is fully instantiated.  In a given execution of this function, there
-- will be exactly one invocation of exactly one of the arguments.
prerender :: forall js m a. Prerender js m => m a -> (PrerenderClientConstraint js m => m a) -> m a
prerender server client = case prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)) of
  Nothing -> server
  Just Dict -> client

instance (HasJS js m, HasJS js (Performable m), HasWebView m, HasWebView (Performable m), MonadFix m, MonadFix (Performable m), ReflexHost t) => Prerender js (ImmediateDomBuilderT t m) where
  prerenderClientDict = Just Dict

data NoJavaScript -- This type should never have a HasJS instance

instance js ~ NoJavaScript => Prerender js (StaticDomBuilderT t m) where
  prerenderClientDict = Nothing

instance Prerender js m => Prerender js (FocusWidget env t m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict @js @m)

instance (Prerender js m, ReflexHost t) => Prerender js (PostBuildT t m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict @js @m)

instance Prerender js m => Prerender js (DynamicWriterT t w m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict @js @m)

instance Prerender js m => Prerender js (ReaderT w m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict @js @m)

instance Prerender js m => Prerender js (RequesterT t request response m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict @js @m)

instance Prerender js m => Prerender js (QueryT t q m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict @js @m)

instance Prerender js m => Prerender js (InputDisabledT m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict @js @m)

instance Prerender js m => Prerender js (PreprocessedCSSClassesT m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict @js @m)
