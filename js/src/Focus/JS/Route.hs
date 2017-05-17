{-# LANGUAGE CPP, OverloadedStrings, RankNTypes #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
module Focus.JS.Route where

import Foreign.JavaScript.TH
import Focus.Request
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding
import qualified Data.Text as T
import Data.Default
import Data.Aeson
import Network.HTTP.Types.URI
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)

#ifdef USE_TEMPLATE_HASKELL
importJS Unsafe "window['location']['search']" "getWindowLocationSearch_" [t| forall x m. MonadJS x m => m T.Text |]
#else
getWindowLocationSearch_ :: MonadJS x m => m T.Text
getWindowLocationSearch_ = runJS (JSFFI "window['location']['search']") [] >>= fromJS
#endif

#ifdef ghcjs_HOST_OS

getWindowLocationSearch :: forall x m. MonadJS x m => m T.Text
getWindowLocationSearch = getWindowLocationSearch_

#else

--TODO: Use the webview JS context here, somehow
getWindowLocationSearch :: forall x m. MonadJS x m => m T.Text
getWindowLocationSearch = return ""

#endif

getRoute :: (HasJS x m, FromJSON r, Default r) => m r
getRoute = do
  params <- liftJS getWindowLocationSearch
  return (fromMaybe def (decodeRoute params))

decodeRoute :: (FromJSON r) => T.Text -> Maybe r
decodeRoute t = do
  Just v <- Map.lookup (encodeUtf8 "x") (Map.fromList (parseQuery (encodeUtf8 t)))
  decodeValue' (LBS.fromStrict v)

-- NB: Nothing represents keys without values, e.g. ?...&foo&...
getLocationParams :: (HasJS x m) => m [(BS.ByteString, Maybe BS.ByteString)]
getLocationParams = fmap (parseQuery . encodeUtf8) (liftJS getWindowLocationSearch)

getLocationParamMap :: (HasJS x m) => m (Map BS.ByteString (Maybe BS.ByteString))
getLocationParamMap = fmap Map.fromList getLocationParams
