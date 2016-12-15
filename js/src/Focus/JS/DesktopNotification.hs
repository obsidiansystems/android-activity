{-# LANGUAGE TemplateHaskell, RankNTypes, OverloadedStrings #-}
module Focus.JS.DesktopNotification where

import Foreign.JavaScript.TH
import Data.Default
import Data.Maybe
import Data.Text
import Data.Monoid

importJS Unsafe "Notification['requestPermission']()" "notificationRequestPermission" [t| forall x m. MonadJS x m => m () |]
importJS Unsafe "Notification['permission']" "notificationPermission" [t| forall x m. MonadJS x m => m Text |]
importJS Unsafe ("(function(that){ " <>
                 "o = {}; " <>
                 "o['body'] = that[1]; " <>
                 "if (that[2] !== '') { o['image'] = that[2]; }; " <>
                 "if (that[3] !== '') { o['icon'] = that[3]; }; " <>
                 "if (that[4] !== '') { o['badge'] = that[4]; }; " <>
                 "var n = new Notification(that[0], o); " <>
                 -- add the "default behavior" back into chrome, and then add dismissal on click
                 "n['onclick'] = function(event) { window['focus'](); n['close'](); };" <>
                 "return n; " <>
                 "})(this)") "newDesktopNotification_" [t| forall x m. MonadJS x m => Text -> Text -> Text -> Text -> Text -> m () |]
importJS Unsafe "window['Notification'] !== undefined" "notificationSupported" [t| forall x m. MonadJS x m => m Bool |]

data DesktopNotificationConfig = DesktopNotificationConfig { _desktopNotificationConfig_image :: Maybe Text
                                                           , _desktopNotificationConfig_icon :: Maybe Text
                                                           , _desktopNotificationConfig_badge :: Maybe Text
                                                           }

instance Default DesktopNotificationConfig where def = DesktopNotificationConfig Nothing Nothing Nothing

newDesktopNotification :: MonadJS x m => Text -> Text -> DesktopNotificationConfig -> m ()
newDesktopNotification title body (DesktopNotificationConfig { _desktopNotificationConfig_image = image
                                                             , _desktopNotificationConfig_icon = icon
                                                             , _desktopNotificationConfig_badge = badge
                                                             }) =
  newDesktopNotification_ title body (fromMaybe "" image) (fromMaybe "" icon) (fromMaybe "" badge)

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
JS(newNotification_, "new Notification($1, {body: $2, icon: $3})", JSRef Text -> JSRef Text -> JSRef Text -> IO ())

newDesktopNotification :: Text -> Text -> Text -> IO ()
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

