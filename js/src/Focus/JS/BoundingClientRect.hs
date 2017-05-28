{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif

module Focus.JS.BoundingClientRect where

import GHCJS.DOM.DOMRectReadOnly
import qualified GHCJS.DOM.Element as Element
import GHCJS.DOM.Types (MonadJSM)
import GHCJS.DOM.Types hiding (Text)

data BoundingClientRect = BoundingClientRect
  { _boundingClientRect_top :: Double
  , _boundingClientRect_right :: Double
  , _boundingClientRect_bottom :: Double
  , _boundingClientRect_left :: Double
  , _boundingClientRect_width :: Double
  , _boundingClientRect_height :: Double
  }
  deriving (Show, Read, Eq, Ord)

getBoundingClientRect :: (IsElement e, MonadJSM m) => e -> m BoundingClientRect
getBoundingClientRect e = do
  x <- Element.getBoundingClientRect e
  top <- getTop x
  bottom <- getBottom x
  right <- getRight x
  left <- getLeft x
  width <- getWidth x
  height <- getHeight x
  return $ BoundingClientRect top right bottom left width height

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
