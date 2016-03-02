{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Focus.JS.BaseJSInstances where

import Reflex.Dom
import Reflex.Spider.Internal (SpiderHostFrame)
import Control.Monad.IO.Class (liftIO)

#ifdef __GHCJS__
instance HasJS JSCtx_IO (Widget Spider (Gui Spider (WithWebView x SpiderHost) x SpiderHostFrame)) where
  type JSM (Widget Spider (Gui Spider (WithWebView x SpiderHost) x SpiderHostFrame)) = IO
  liftJS = liftIO

instance HasJS JSCtx_IO (Gui Spider (WithWebView x SpiderHost) x SpiderHostFrame) where
  type JSM (Gui Spider (WithWebView x SpiderHost) x SpiderHostFrame) = IO
  liftJS = liftIO

instance HasJS JSCtx_IO (WithWebView x SpiderHost) where
  type JSM (WithWebView x SpiderHost) = IO
  liftJS = liftIO

#else
instance HasJS (JSCtx_JavaScriptCore x) (Widget Spider (Gui Spider (WithWebView x SpiderHost) x SpiderHostFrame)) where
  type JSM (Widget Spider (Gui Spider (WithWebView x SpiderHost) x SpiderHostFrame)) = WithWebView x IO
  liftJS a = do
    wv <- askWebView
    liftIO $ runWithWebView a wv

instance HasJS (JSCtx_JavaScriptCore x) (Gui Spider (WithWebView x SpiderHost) x SpiderHostFrame) where
  type JSM (Gui Spider (WithWebView x SpiderHost) x SpiderHostFrame) = WithWebView x IO
  liftJS a = do
    wv <- askWebView
    liftIO $ runWithWebView a wv
#endif

