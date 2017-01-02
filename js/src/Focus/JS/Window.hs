{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Focus.JS.Window where

import Foreign.JavaScript.TH
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.Window as DOM
import Reflex.Dom
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix

askDomWindow :: (HasWebView m, MonadIO m) => m DOM.Window
askDomWindow = do
  wv <- askWebView
  Just doc <- liftIO $ DOM.webViewGetDomDocument $ unWebViewSingleton wv
  Just window <- liftIO $ DOM.getDefaultView doc
  return window

windowHasFocus :: ( Reflex t
                  , MonadHold t m
                  , TriggerEvent t m
                  )
               => DOM.Window
               -> m (Dynamic t Bool)
windowHasFocus w = do
  f <- wrapDomEvent w (`DOM.on` DOM.focusEvent) $ return True
  b <- wrapDomEvent w (`DOM.on` DOM.blurEvent) $ return False
  holdDyn True $ leftmost [f, b] --TODO: Get the initial value properly

windowResizeEvent :: ( Reflex t
                     , TriggerEvent t m )
                  => DOM.Window
                  -> m (Event t (Int, Int))
windowResizeEvent w = do
  wrapDomEvent w (`DOM.on` DOM.resize) $ do
    width <- DOM.getInnerWidth w
    height <- DOM.getInnerHeight w
    return (width, height)

getWindowSize :: DOM.Window -> IO (Int, Int)
getWindowSize w = liftM2 (,) (DOM.getInnerWidth w) (DOM.getInnerHeight w)

windowSize :: ( Reflex t
              , TriggerEvent t m
              , MonadFix m
              , PostBuild t m
              , MonadIO (Performable m)
              , PerformEvent t m
              , MonadHold t m )
           => DOM.Window
           -> m (Dynamic t (Int, Int))
windowSize w = do
  resizeRaw <- windowResizeEvent w
  resize <- debounce 0.25 resizeRaw -- debounce because the raw resize event can be very spammy in some browsers when a user is dragging
  pb <- getPostBuild
  initialSize <- performEvent (liftIO (getWindowSize w) <$ pb)
  holdDyn (0,0) $ leftmost [resize, initialSize]