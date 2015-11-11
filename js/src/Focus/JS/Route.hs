{-# LANGUAGE TemplateHaskell, RankNTypes, PatternGuards #-}
module Focus.JS.Route where

import Foreign.JavaScript.TH
import Focus.Request
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding
import qualified Data.Text as T
import Data.Default
import Data.Aeson

importJS Unsafe "decodeURIComponent(window['location']['search'])" "getWindowLocationSearch" [t| forall x m. MonadJS x m => m String |]

getRoute :: (HasJS x m, FromJSON r, Default r) => m r
getRoute = do
  search <- liftJS getWindowLocationSearch
  let searchPrefix = "?x="
  return $ case take (length searchPrefix) search of
    "?x="
      | Just x <- decodeValue' $ LBS.fromStrict $ encodeUtf8 $ T.pack $ drop (length searchPrefix) search
        -> x
    _ -> def
