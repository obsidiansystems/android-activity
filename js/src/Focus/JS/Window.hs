module Focus.JS.Window where

import Foreign.JavaScript.TH
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.EventM
import GHCJS.DOM.Window
import Reflex.Dom
import Reflex.Host.Class
import Control.Monad.IO.Class

askDomWindow :: (HasWebView m, MonadIO m) => m Window
askDomWindow = do
  wv <- askWebView
  Just doc <- liftIO $ webViewGetDomDocument $ unWebViewSingleton wv
  Just window <- liftIO $ getDefaultView doc
  return window

windowHasFocus :: ( Reflex t, MonadHold t m
                  , HasPostGui t h m, MonadReflexCreateTrigger t m
                  , MonadIO m
                  )
               => Window
               -> m (Dynamic t Bool)
windowHasFocus w = do
  f <- wrapDomEvent w (`on` focusEvent) $ return True
  b <- wrapDomEvent w (`on` blurEvent) $ return False
  holdDyn True $ leftmost [f, b]

