module Focus.JS.Listen where

import Reflex.Class

accumNotifications :: MonadHoldEvent t m => (a -> b -> b) -> b -> (n -> Maybe a) -> Event t n -> m (Dynamic t b)
accumNotifications g z f e = foldDyn g z =<< mapMaybeE f e
