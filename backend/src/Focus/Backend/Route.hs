{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleInstances, OverloadedStrings #-}
module Focus.Backend.Route
       ( module Focus.Backend.Route
       , module Focus.Route
       ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Maybe
import Focus.Route
import Focus.Request
import Snap.Core

getRouteSnap :: (MonadSnap m, FromJSON r, Default r) => m r
getRouteSnap = do
  mRouteRaw <- getQueryParam "x" --TODO: Factor out "x"
  return $ fromMaybe def $ decodeValue' . LBS.fromStrict =<< mRouteRaw
