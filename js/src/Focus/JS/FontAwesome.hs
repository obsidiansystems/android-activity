module Focus.JS.FontAwesome where

import Reflex.Dom
import Data.Monoid

icon :: MonadWidget t m => String -> m ()
icon i = elClass "i" ("fa fa-" <> i) $ return ()

icon2x :: MonadWidget t m => String -> m ()
icon2x i = icon (i <> " fa-2x")
