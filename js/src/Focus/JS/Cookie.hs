{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Focus.JS.Cookie where

import Control.Monad.IO.Class
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (toLazyByteString)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar
import qualified GHCJS.DOM.Document as DOM
import Reflex.Dom
import Web.Cookie

setPermanentCookie :: (MonadIO m, HasWebView m) => DOM.Document -> Text -> Maybe Text -> m ()
setPermanentCookie doc key mv = do
  wv <- askWebView
  currentProtocol <- Reflex.Dom.getLocationProtocol wv
  DOM.setCookie doc . Just . decodeUtf8 . LBS.toStrict . toLazyByteString . renderSetCookie $ case mv of
    Nothing -> def
      { setCookieName = encodeUtf8 key
      , setCookieValue = ""
      , setCookieExpires = Just $ posixSecondsToUTCTime 0
      }
    Just val -> def
      { setCookieName = encodeUtf8 key
      , setCookieValue = encodeUtf8 val
      -- | We don't want these to expire, but browsers don't support
      -- non-expiring cookies.  Some systems have trouble representing dates
      -- past 2038, so use 2037.
      , setCookieExpires = Just $ UTCTime (fromGregorian 2037 1 1) 0
      , setCookieSecure = currentProtocol == "https:"
      -- | This helps prevent CSRF attacks; we don't want strict, because it
      -- would prevent links to the page from working; lax is secure enough,
      -- because we don't take dangerous actions simply by executing a GET
      -- request.
      , setCookieSameSite = Just sameSiteLax
      }

-- | Retrieve the current auth token from the given cookie
getCookie :: MonadIO m => DOM.Document -> Text -> m (Maybe Text)
getCookie doc key = do
  Just cookieString <- DOM.getCookie doc
  return $ lookup key $ parseCookiesText $ encodeUtf8 cookieString

setPermanentCookieJson :: (MonadIO m, HasWebView m, ToJSON v) => DOM.Document -> Text -> Maybe v -> m ()
setPermanentCookieJson d k v =
  setPermanentCookie d k (fmap (decodeUtf8 . LBS.toStrict . encode) v)

getCookieJson :: (FromJSON v, MonadIO m) => DOM.Document -> Text -> m (Maybe (Either String v))
getCookieJson d k =
  fmap (eitherDecode . LBS.fromStrict . encodeUtf8) <$> getCookie d k

withPermanentCookieJson :: (MonadIO m, MonadIO (Performable m), HasWebView (Performable m), PerformEvent t m, ToJSON v, FromJSON v)
                        => DOM.Document
                        -> Text
                        -> (Maybe (Either String v) -> m (Event t (Maybe v)))
                        -> m ()
withPermanentCookieJson d k a = do
  cookie0 <- getCookieJson d k
  cookieE <- a cookie0
  performEvent_ $ setPermanentCookieJson d k <$> cookieE
  return ()
