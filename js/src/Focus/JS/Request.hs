{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, GADTs, ScopedTypeVariables, FunctionalDependencies, RecursiveDo, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, EmptyDataDecls, NoMonomorphismRestriction, TemplateHaskell, PolyKinds, TypeOperators, DeriveFunctor, LambdaCase, CPP, ForeignFunctionInterface, JavaScriptFFI, DeriveDataTypeable, ConstraintKinds #-}
module Focus.JS.Request where

import GHCJS.DOM
import GHCJS.DOM.DOMWindow
import GHCJS.DOM.Types hiding (Event, XMLHttpRequest)
import GHCJS.DOM.Document
import GHCJS.DOM.Node
import GHCJS.DOM.NamedNodeMap
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLDocument
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.HTMLSelectElement
import GHCJS.DOM.HTMLOptionElement
import GHCJS.DOM.MouseEvent
import GHCJS.DOM.UIEvent
import GHCJS.DOM.NodeList
import GHCJS.DOM.Event hiding (Event)
import GHCJS.DOM.EventM (event, Signal (..), preventDefault)
import qualified GHCJS.DOM.EventM as GHCJS
import GHCJS.DOM.EventTargetClosures
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid
import Data.Time.Clock
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.String
import Data.Aeson
import Control.Concurrent.MVar
import Control.Monad
import Foreign.JavaScript.TH
import Foreign.ForeignPtr

{-
-- Note: The C preprocessor will fail if you use a single-quote in the name
#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

-}

validJSRef :: MonadJS x m => JSRef x -> m (Maybe (JSRef x))
validJSRef r = do
  u <- isJSUndefined r
  n <- isJSNull r
  return $ if u || n then Nothing else Just r

timeFrom t ct =
  let d = round $ diffUTCTime ct t
  in describe d
  where
    describeAbs :: Integer -> String
    describeAbs n
      | n >= 86400 = let days = n `Prelude.div` 86400 in show days <> " days "
      | n >= 3600 = let hrs = n `Prelude.div` 3600 in show hrs <> " hours "
      | n >= 60 = let mins = n `Prelude.div` 60 in show mins <> " minutes "
      | n > 0 = show n <> " seconds "
      | otherwise = ""
    describe :: Integer -> String
    describe n = if n > 0
                    then describeAbs n <> "ago"
                    else describeAbs (abs n) <> "from now"
