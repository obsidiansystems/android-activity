module Focus.JS.Listen where

import Prelude hiding (foldl1)

import Control.Monad.Fix
import Data.List.NonEmpty (nonEmpty)
import Data.Semigroup
import Data.Foldable
import Reflex
import Data.Maybe

accumNotifications :: (Control.Monad.Fix.MonadFix m, Reflex t, MonadHold t m) => (a -> b -> b) -> b -> (n -> Maybe a) -> Event t [n] -> m (Dynamic t b)
accumNotifications g z f e = foldDyn (foldl1 (.) . fmap g) z $ fmapMaybe (nonEmpty . catMaybes . map f) e
