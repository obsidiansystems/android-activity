{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Focus.JS.Account where

import Focus.JS.LocalStorage

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (toLazyByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar
import Focus.Account
import Focus.Request
import Focus.Sign
import GHCJS.DOM.Types (MonadJSM)
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Storage as DOM
import Reflex.Dom.Core
import Web.Cookie

--TODO: A more general cookie API is in Focus.JS.Cookie and this module should use that for cookie management

-- | Run a widget with an auth
withPermanentAuthTokenFromCookie :: (MonadJSM m, HasJSContext m, MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m)
                                    => DOM.Document
                                 -> Text
                                 -> (Maybe (Signed (AuthToken f)) -> m (Event t (Maybe (Signed (AuthToken f)))))
                                 -> m ()
withPermanentAuthTokenFromCookie doc key a = do
  -- Try a couple of ways of getting the initial auth token
  authToken0 <- runMaybeT $ msum $ MaybeT <$>
    [ getAuthTokenCookie doc key
    , migrateLocalStorageAuthTokenToCookies doc key
    ]
  tokenE <- a authToken0
  performEvent_ $ setPermanentAuthTokenCookie doc key <$> tokenE
  return ()

setPermanentAuthTokenCookie :: (MonadJSM m, HasJSContext m) => DOM.Document -> Text -> Maybe (Signed (AuthToken f)) -> m ()
setPermanentAuthTokenCookie doc = setPermanentAuthTokenCookieWithLocation doc Nothing

setPermanentAuthTokenCookieWithLocation :: (MonadJSM m, HasJSContext m) => DOM.Document  -> Maybe ByteString -> Text -> Maybe (Signed (AuthToken f)) -> m ()
setPermanentAuthTokenCookieWithLocation doc loc key mt = do
  currentProtocol <- Reflex.Dom.Core.getLocationProtocol
  DOM.setCookie doc . decodeUtf8 . LBS.toStrict . toLazyByteString . renderSetCookie $ case mt of
    Nothing -> def
      { setCookieName = encodeUtf8 key
      , setCookieValue = ""
      , setCookieExpires = Just $ posixSecondsToUTCTime 0
      , setCookieDomain = loc
      }
    Just (Signed t) -> def
      { setCookieName = encodeUtf8 key
      , setCookieValue = encodeUtf8 t
        -- We don't want these to expire, but browsers don't support
        -- non-expiring cookies.  Some systems have trouble representing dates
        -- past 2038, so use 2037.
      , setCookieExpires = Just $ UTCTime (fromGregorian 2037 1 1) 0
      , setCookieSecure = currentProtocol == "https:"
        -- This helps prevent CSRF attacks; we don't want strict, because it
        -- would prevent links to the page from working; lax is secure enough,
        -- because we don't take dangerous actions simply by executing a GET
        -- request.
      , setCookieSameSite = if currentProtocol == "file:"
          then Nothing
          else Just sameSiteLax
      , setCookieDomain = loc
      }

-- | Retrieve the current auth token from the given cookie
getAuthTokenCookie :: MonadJSM m => DOM.Document -> Text -> m (Maybe (Signed (AuthToken f)))
getAuthTokenCookie doc key = do
  cookieString <- DOM.getCookie doc
  return $ fmap Signed $ lookup key $ parseCookiesText $ encodeUtf8 cookieString

-- | Try to retrieve the auth token from local storage; if we succeed, clear it
-- out of local storage, save it in a cookie, and return it.
migrateLocalStorageAuthTokenToCookies :: (MonadJSM m, HasJSContext m) => DOM.Document -> Text -> m (Maybe (Signed (AuthToken f)))
migrateLocalStorageAuthTokenToCookies doc key = do
  authTokenStr <- storageGetInitial $ T.unpack key
  authToken0 <- case authTokenStr of
    Nothing -> return Nothing
    Just t -> return $ decodeValue' . LBS.fromStrict . encodeUtf8 $ T.pack t
  forM_ authToken0 $ \t -> do
    setPermanentAuthTokenCookie doc key $ Just t
    s <- askLocalStorage
    DOM.removeItem s key
  return authToken0
