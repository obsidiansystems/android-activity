{-# LANGUAGE CPP, OverloadedStrings, RankNTypes #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
module Focus.JS.Route where

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
import Language.Javascript.JSaddle (eval, JSM, valToText, liftJSM, MonadJSM)

getWindowLocationSearch :: JSM T.Text
getWindowLocationSearch = valToText $ eval ("window['location']['search']" :: T.Text)

getRoute :: (MonadJSM m, FromJSON r, Default r) => m r
getRoute = do
  params <- liftJSM getWindowLocationSearch
  return (fromMaybe def (decodeRoute params))

decodeRoute :: (FromJSON r) => T.Text -> Maybe r
decodeRoute t = do
  Just v <- Map.lookup (encodeUtf8 "x") (Map.fromList (parseQuery (encodeUtf8 t)))
  decodeValue' (LBS.fromStrict v)

getWindowLocationPathName :: JSM T.Text
getWindowLocationPathName = valToText $ eval ("window['location']['pathname']" :: T.Text)

getDefaultParam :: FromJSON b => Map BS.ByteString (Maybe BS.ByteString) -> Maybe b
getDefaultParam params = do
  Just v <- Map.lookup (encodeUtf8 "x") params
  decodeValue' (LBS.fromStrict v)

-- NB: Nothing represents keys without values, e.g. ?...&foo&...
getLocationParams :: MonadJSM m => m [(BS.ByteString, Maybe BS.ByteString)]
getLocationParams = fmap (parseQuery . encodeUtf8) (liftJSM getWindowLocationSearch)

getLocationParamMap :: MonadJSM m => m (Map BS.ByteString (Maybe BS.ByteString))
getLocationParamMap = fmap Map.fromList getLocationParams

getRouteWith :: (FromJSON r, Default r, MonadJSM m)
             => (T.Text -> Map BS.ByteString (Maybe BS.ByteString) -> Maybe r)
             -> m r
getRouteWith f = do
  path <- liftJSM getWindowLocationPathName
  params <- getLocationParamMap
  return $ case f path params of
    Nothing -> def
    Just r -> r
