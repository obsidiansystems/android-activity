{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP #-}
module Focus.JS.DesktopNotification where

import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

JS(notificationRequestPermission_, "Notification.requestPermission()", IO ())
JS(notificationPermission_, "Notification.permission", IO JSString)
JS(newNotification_, "new Notification($1, {body: $2, icon: $3})", JSRef String -> JSRef String -> JSRef String -> IO ())

newDesktopNotification :: String -> String -> String -> IO ()
newDesktopNotification title body icon = do
  t <- toJSRef title
  b <- toJSRef body
  i <- toJSRef icon
  newNotification_ t b i

desktopNotificationEnabled :: IO Bool
desktopNotificationEnabled = do
  p <- liftM fromJSString $ notificationPermission_
  return $ case p of
                "granted" -> True
                _ -> False

enableDesktopNotification :: IO ()
enableDesktopNotification = do
  on <- desktopNotificationEnabled
  if on then return ()
        else notificationRequestPermission_

