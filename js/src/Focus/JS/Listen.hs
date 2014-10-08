module Focus.JS.Listen where

import Prelude hiding (foldl1)

import Data.List.NonEmpty (nonEmpty)
import Data.Semigroup
import Data.Foldable
import Reflex.Class
import Data.Maybe

accumNotifications :: MonadHoldEvent t m => (a -> b -> b) -> b -> (n -> Maybe a) -> Event t [n] -> m (Dynamic t b)
accumNotifications g z f e = foldDyn (foldl1 (.) . fmap g) z =<< mapMaybeE (nonEmpty . catMaybes . map f) e
