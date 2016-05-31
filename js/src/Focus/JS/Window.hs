{-# LANGUAGE TypeFamilies #-}
module Focus.JS.Window where

import Foreign.JavaScript.TH
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.Window as DOM
import Reflex.Dom
import Control.Monad.IO.Class

askDomWindow :: (HasWebView m, MonadIO m) => m DOM.Window
askDomWindow = do
  wv <- askWebView
  Just doc <- liftIO $ DOM.webViewGetDomDocument $ unWebViewSingleton wv
  Just window <- liftIO $ DOM.getDefaultView doc
  return window

windowHasFocus :: ( Reflex t
                  , MonadHold t m
                  , TriggerEvent t m
                  , MonadIO m
                  )
               => DOM.Window
               -> m (Dynamic t Bool)
windowHasFocus w = do
  f <- wrapDomEvent w (`DOM.on` DOM.focusEvent) $ return True
  b <- wrapDomEvent w (`DOM.on` DOM.blurEvent) $ return False
  holdDyn True $ leftmost [f, b] --TODO: Get the initial value properly
