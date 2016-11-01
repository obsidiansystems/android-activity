{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Focus.HereMaps where

import Control.Exception
import Data.Text (Text, unpack)
import Data.Aeson
import Data.Aeson.TH
import Data.Int
import Data.Char
import Data.Monoid
import Data.Time.Clock
import Network.HTTP.Conduit

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

-- Returns distance between two points in meters (taking the route into account)
distanceReq :: HereMapsCredentials -> (Double, Double) -> (Double, Double) -> IO (Maybe Int64)
distanceReq creds (lat1, lng1) (lat2, lng2) = do
  tstart <- getCurrentTime
  let url = "https://route.api.here.com/routing/7.2/calculateroute.json?waypoint0=" <> show lat1 <> "%2C" <> show lng1 <> "&waypoint1=" <> show lat2 <> "%2C" <> show lng2 <> "&mode=fastest%3Btruck%3Btraffic%3Adisabled" <> hereMapsCredentialsQueryString creds
  resp <- try $ simpleHttp url
  tend <- getCurrentTime
  putStrLn $ "distanceReq: HERE.com API Call returned in " ++ show (diffUTCTime tend tstart) ++ " [" ++ url ++ "]"
  case resp of
    Left (SomeException e) -> do
      putStrLn ("distanceReq: Error while connecting to the here.com API: " ++ show e)
      return Nothing
    Right routeResp -> do
      let result = do
          response <- decode routeResp
          route <- safeHead $ routeResponse_route $ rawResponse_response response
          summary <- routeRouteResponse_summary route
          return $ routeRouteSummaryResponse_distance summary
      return $! result
  where
    safeHead [] = Nothing
    safeHead (a:_) = Just a

-- Geocode an address into a coordinate
geocodeReq :: HereMapsCredentials -> String -> IO (Maybe (Double, Double))
geocodeReq creds address = do
  tstart <- getCurrentTime
  let url = "https://geocoder.api.here.com/6.2/geocode.json?searchtext=" <> address <> hereMapsCredentialsQueryString creds
  resp <- try $ simpleHttp url
  tend <- getCurrentTime
  putStrLn $ "geocodeReq: HERE.com API Call returned in " ++ show (diffUTCTime tend tstart) ++ " [" ++ url ++ "]"
  case resp of
    Left (SomeException e) -> do
      putStrLn ("geocodeReq: Error while connecting to the here.com API: " ++ show e)
      return Nothing
    Right geocodeResp -> do
      let result = do
          response <- decode geocodeResp
          view <- safeHead $ viewResponse_view $ rawResponse_response response
          location <- safeHead $ resultResponse_result view
          let DisplayPositionCoordsResponse lat lon = locationResponse_displayPosition $ resultLocationResponse_location location
          return $ (lat, lon)
      -- return result
      return result
  where
    safeHead [] = Nothing
    safeHead (a:_) = Just a

hereMapsCredentialsQueryString :: HereMapsCredentials -> String
hereMapsCredentialsQueryString creds = "&app_id=" ++ unpack (_hereMapsCredentials_appId creds) ++ "&app_code=" ++ unpack (_hereMapsCredentials_appCode creds)
