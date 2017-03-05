{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Focus.HereMaps where

import Data.Text (Text, unpack)
import Data.Aeson
import Data.Aeson.TH
import Data.Int

import Data.Char
import Focus.Request (makeJson)

-- API CREDENTIALS
data HereMapsCredentials = HereMapsCredentials
  { _hereMapsCredentials_appId :: Text
  , _hereMapsCredentials_appCode :: Text
  } deriving (Show, Read, Eq, Ord)

makeJson ''HereMapsCredentials

-- JSON Data Structures
data RouteRouteSummaryResponse = RouteRouteSummaryResponse
  { routeRouteSummaryResponse_distance :: Int64 -- ^ Total route distance in meters
  -- There are other fields we don't care about
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON (defaultOptions { fieldLabelModifier = drop 26 }) ''RouteRouteSummaryResponse

data RouteRouteResponse = RouteRouteResponse
  { routeRouteResponse_summary :: Maybe RouteRouteSummaryResponse
  -- There are other fields we don't care about
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON (defaultOptions { fieldLabelModifier = drop 19 }) ''RouteRouteResponse

data RouteResponse = RouteResponse
  { routeResponse_route :: [RouteRouteResponse]
  -- There are other fields we don't care about
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON (defaultOptions { fieldLabelModifier = drop 14 }) ''RouteResponse

data RawRouteResponse = RawRouteResponse
  { rawRouteResponse_response :: RouteResponse
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON (defaultOptions { fieldLabelModifier = drop ((length :: String -> Int) "rawRouteResponse_") }) ''RawRouteResponse


data DisplayPositionCoordsResponse = DisplayPositionCoordsResponse
  { displayPositionCoordsResponse_latitude :: Double
  , displayPositionCoordsResponse_longitude :: Double
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON (defaultOptions { fieldLabelModifier = ((:) . toUpper . head <*> tail) . drop ((length :: String -> Int) "displayPositionCoordsResponse_") }) ''DisplayPositionCoordsResponse

data LocationResponse = LocationResponse
  { locationResponse_displayPosition :: DisplayPositionCoordsResponse
  -- There are other fields we don't care about
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON (defaultOptions { fieldLabelModifier = ((:) . toUpper . head <*> tail) . drop ((length :: String -> Int) "locationResponse_") }) ''LocationResponse

data ResultLocationResponse = ResultLocationResponse
  { resultLocationResponse_location :: LocationResponse
  -- There are other fields we don't care about
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON (defaultOptions { fieldLabelModifier = ((:) . toUpper . head <*> tail) . drop ((length :: String -> Int) "resultLocationResponse_") }) ''ResultLocationResponse

data ResultResponse = ResultResponse
  { resultResponse_result :: [ResultLocationResponse]
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON (defaultOptions { fieldLabelModifier = ((:) . toUpper . head <*> tail) . drop ((length :: String -> Int) "resultResponse_") }) ''ResultResponse

data ViewResponse = ViewResponse
  { viewResponse_view :: [ResultResponse]
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON (defaultOptions { fieldLabelModifier = ((:) . toUpper . head <*> tail) . drop ((length :: String -> Int) "viewResponse_") }) ''ViewResponse

data RawResponse a = RawResponse
  { rawResponse_response :: a
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON (defaultOptions { fieldLabelModifier = ((:) . toUpper . head <*> tail) . drop ((length :: String -> Int) "rawResponse_") }) ''RawResponse

hereMapsCredentialsQueryString :: HereMapsCredentials -> String
hereMapsCredentialsQueryString creds = "&app_id=" ++ unpack (_hereMapsCredentials_appId creds) ++ "&app_code=" ++ unpack (_hereMapsCredentials_appCode creds)

