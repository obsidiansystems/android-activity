{-# LANGUAGE OverloadedStrings #-}
module Focus.Backend.HereMaps
  ( module Focus.Backend.HereMaps
  , module Focus.HereMaps
  ) where

import Control.Exception
import Data.Aeson
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Int
import Network.HTTP.Conduit
import Network.URI

import Focus.HereMaps

-- Returns distance between two points in meters (taking the route into account)
distanceReq :: Manager -> HereMapsCredentials -> (Double, Double) -> (Double, Double) -> IO (Maybe Int64)
distanceReq mgr creds (lat1, lng1) (lat2, lng2) = do
  tstart <- getCurrentTime
  let url = "https://route.api.here.com/routing/7.2/calculateroute.json?waypoint0=" <> show lat1 <> "%2C" <> show lng1 <> "&waypoint1=" <> show lat2 <> "%2C" <> show lng2 <> "&mode=fastest%3Btruck%3Btraffic%3Adisabled" <> hereMapsCredentialsQueryString creds
  req <- parseUrlThrow url
  resp <- try $ responseBody <$> httpLbs (req { requestHeaders = ("Connection", "close") : requestHeaders req }) mgr
  tend <- getCurrentTime
  putStrLn $ "distanceReq: HERE.com API Call returned in " ++ show (diffUTCTime tend tstart) ++ " [" ++ url ++ "]"
  case resp of
    Left (SomeException e) -> do
      putStrLn ("distanceReq: Error while connecting to the here.com API: " ++ show e)
      return Nothing
    Right routeResp -> do
      let result = do
          response <- decode routeResp
          route <- listToMaybe $ routeResponse_route $ rawRouteResponse_response response
          summary <- routeRouteResponse_summary route
          return $ routeRouteSummaryResponse_distance summary
      return $! result

data GeocodeRequest = GeocodeRequest
  { _geocodeRequest_searchText :: Maybe Text
  , _geocodeRequest_city :: Maybe Text
  , _geocodeRequest_state :: Maybe Text
  , _geocodeRequest_country :: Maybe Text
  }

-- | Geocode an address into a coordinate
geocodeReq :: Manager -> HereMapsCredentials -> GeocodeRequest -> IO (Maybe (Double, Double))
geocodeReq mgr creds geoReq = do
  tstart <- getCurrentTime
  let escape = escapeURIString isUnreserved . T.unpack
      query' = intercalate "&" $ catMaybes $ map (\(k, mv) -> mv >>= \v -> return $ escape k <> "=" <> escape v)
        [ ("searchtext", _geocodeRequest_searchText geoReq)
        , ("city", _geocodeRequest_city geoReq)
        , ("country", _geocodeRequest_country geoReq)
        , ("state", _geocodeRequest_state geoReq)
        , ("app_id", Just $ _hereMapsCredentials_appId creds)
        , ("app_code", Just $ _hereMapsCredentials_appCode creds)
        ]
  let url = show $ nullURI
        { uriScheme = "https:"
        , uriAuthority = Just $ URIAuth "" "geocoder.api.here.com" ""
        , uriPath = "/6.2/geocode.json"
        , uriQuery = "?" <> query'
        }
  req <- parseUrlThrow url
  resp <- try $ responseBody <$> httpLbs (req { requestHeaders = ("Connection", "close") : requestHeaders req }) mgr
  tend <- getCurrentTime
  putStrLn $ "geocodeReq: HERE.com API Call returned in " ++ show (diffUTCTime tend tstart) ++ " [" ++ url ++ "]"
  case resp of
    Left (SomeException e) -> do
      putStrLn ("geocodeReq: Error while connecting to the here.com API: " ++ show e)
      return Nothing
    Right geocodeResp -> return $ do
      response <- decode geocodeResp
      view <- listToMaybe $ viewResponse_view $ rawResponse_response response
      location <- listToMaybe $ resultResponse_result view
      let DisplayPositionCoordsResponse lat lon = locationResponse_displayPosition $ resultLocationResponse_location location
      return (lat, lon)
