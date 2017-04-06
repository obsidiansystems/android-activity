{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif

module Focus.JS.BoundingClientRect where

import Control.Monad
import Data.Text (Text)
import GHCJS.DOM.ClientRect
#ifdef ghcjs_HOST_OS
import qualified GHCJS.DOM.Element as Element
#endif
import GHCJS.DOM.Types (MonadJSM)
import GHCJS.DOM.Types hiding (Text)
import Language.Javascript.JSaddle.Evaluate
import Language.Javascript.JSaddle.Object

data BoundingClientRect = BoundingClientRect
  { _boundingClientRect_top :: Float
  , _boundingClientRect_right :: Float
  , _boundingClientRect_bottom :: Float
  , _boundingClientRect_left :: Float
  , _boundingClientRect_width :: Float
  , _boundingClientRect_height :: Float
  }
  deriving (Show, Read, Eq, Ord)

getBoundingClientRect :: (IsElement e, MonadJSM m) => e -> m (Maybe BoundingClientRect)
#ifdef ghcjs_HOST_OS
getBoundingClientRect e = do
  x <- Element.getBoundingClientRect e
  forM x $ \x' -> do
    top <- getTop x'
    bottom <- getBottom x'
    right <- getRight x'
    left <- getLeft x'
    width <- getWidth x'
    height <- getHeight x'
    return $ BoundingClientRect top right bottom left width height
#else
getBoundingClientRect e = liftJSM $ do
  f <- eval ("(function (e) { return e.getBoundingClientRect(); })" :: Text)
  x <- fromJSVal =<< call f f [e]
  forM x $ \x' -> do
    top <- getTop x'
    bottom <- getBottom x'
    right <- getRight x'
    left <- getLeft x'
    width <- getWidth x'
    height <- getHeight x'
    return $ BoundingClientRect top right bottom left width height
#endif

invisible :: BoundingClientRect
invisible = BoundingClientRect
  { _boundingClientRect_top = 0
  , _boundingClientRect_right = 0
  , _boundingClientRect_bottom = 0
  , _boundingClientRect_left = 0
  , _boundingClientRect_width = 0
  , _boundingClientRect_height = 0
  }

clientRectIsVisible :: BoundingClientRect -- ^ Viewport bounds
                    -> BoundingClientRect -- ^ Element bounds
                    -> Bool -- ^ Whether any part of the element is visible in the viewport
clientRectIsVisible vp c = not $
  _boundingClientRect_top vp > _boundingClientRect_bottom c ||
  _boundingClientRect_bottom vp < _boundingClientRect_top c
