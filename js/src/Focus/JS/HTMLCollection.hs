{-# LANGUAGE RankNTypes #-}
module Focus.JS.HTMLCollection where

import Control.Monad
import qualified GHCJS.DOM.HTMLCollection as HC
import GHCJS.DOM.HTMLElement (getChildrenUnchecked)
import GHCJS.DOM.Types

mapChildren :: forall e a. (IsHTMLElement e) => (HTMLElement -> JSM a) -> e -> JSM [a]
mapChildren f e = do
  children <- getChildrenUnchecked e
  n <- HC.getLength children
  forM [0 .. n-1] $ \ix -> do
    c <- HC.itemUnchecked children ix
    f $ uncheckedCastTo HTMLElement c

forChildren :: forall e a. (IsHTMLElement e) => e -> (HTMLElement -> JSM a) -> JSM [a]
forChildren = flip mapChildren
