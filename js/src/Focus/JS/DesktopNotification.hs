{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Focus.JS.DesktopNotification where

import Foreign.JavaScript.TH

importJS Unsafe "Notification.requestPermission()" "notificationRequestPermission" [t| forall x m. MonadJS x m => m () |]
importJS Unsafe "Notification.permission" "notificationPermission" [t| forall x m. MonadJS x m => m String |]
importJS Unsafe "new Notification(this[0], {body: this[1], icon: this[2]})" "newDesktopNotification" [t| forall x m. MonadJS x m => String -> String -> String -> m () |]
importJS Unsafe "window.Notification !== undefined" "notificationSupported" [t| forall x m. MonadJS x m => m Bool |]

desktopNotificationEnabled :: MonadJS x m => m Bool
desktopNotificationEnabled = do
  p <- notificationPermission
  return $ p == "granted"

{-
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
-}

enableDesktopNotification :: MonadJS x m => m ()
enableDesktopNotification = do
  on <- desktopNotificationEnabled
  if on then return ()
        else notificationRequestPermission

