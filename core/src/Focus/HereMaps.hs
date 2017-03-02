{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, OverloadedStrings #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
module Focus.HereMaps where

{-

import Data.Text (Text, unpack)
import Data.Aeson
import Data.Aeson.TH
import Data.Int
#ifdef FOCUS_BACKEND
import Control.Exception
import qualified Data.Text as T
import Data.Monoid
import Data.Time.Clock
import Network.HTTP.Conduit
#endif

#ifdef USE_TEMPLATE_HASKELL
import Data.Char
import Focus.Request (makeJson)
#endif
#if !defined(USE_TEMPLATE_HASKELL) || defined(DUMPING_SPLICES)
import Data.Aeson.Encoding.Internal
import Data.Text (pack)
import Data.List (intersperse)
import Focus.Request (HList(..))
#endif

-- API CREDENTIALS
data HereMapsCredentials = HereMapsCredentials
  { _hereMapsCredentials_appId :: Text
  , _hereMapsCredentials_appCode :: Text
  } deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
makeJson ''HereMapsCredentials
#endif

-- JSON Data Structures
data RouteRouteSummaryResponse = RouteRouteSummaryResponse
  { routeRouteSummaryResponse_distance :: Int64 -- ^ Total route distance in meters
  -- There are other fields we don't care about
  }
  deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
deriveJSON (defaultOptions { fieldLabelModifier = drop 26 }) ''RouteRouteSummaryResponse
#endif

data RouteRouteResponse = RouteRouteResponse
  { routeRouteResponse_summary :: Maybe RouteRouteSummaryResponse
  -- There are other fields we don't care about
  }
  deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
deriveJSON (defaultOptions { fieldLabelModifier = drop 19 }) ''RouteRouteResponse
#endif

data RouteResponse = RouteResponse
  { routeResponse_route :: [RouteRouteResponse]
  -- There are other fields we don't care about
  }
  deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
deriveJSON (defaultOptions { fieldLabelModifier = drop 14 }) ''RouteResponse
#endif

data RawRouteResponse = RawRouteResponse
  { rawRouteResponse_response :: RouteResponse
  }
  deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
deriveJSON (defaultOptions { fieldLabelModifier = drop ((length :: String -> Int) "rawRouteResponse_") }) ''RawRouteResponse
#endif


data DisplayPositionCoordsResponse = DisplayPositionCoordsResponse
  { displayPositionCoordsResponse_latitude :: Double
  , displayPositionCoordsResponse_longitude :: Double
  }
  deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
deriveJSON (defaultOptions { fieldLabelModifier = ((:) . toUpper . head <*> tail) . drop ((length :: String -> Int) "displayPositionCoordsResponse_") }) ''DisplayPositionCoordsResponse
#endif

data LocationResponse = LocationResponse
  { locationResponse_displayPosition :: DisplayPositionCoordsResponse
  -- There are other fields we don't care about
  }
  deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
deriveJSON (defaultOptions { fieldLabelModifier = ((:) . toUpper . head <*> tail) . drop ((length :: String -> Int) "locationResponse_") }) ''LocationResponse
#endif

data ResultLocationResponse = ResultLocationResponse
  { resultLocationResponse_location :: LocationResponse
  -- There are other fields we don't care about
  }
  deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
deriveJSON (defaultOptions { fieldLabelModifier = ((:) . toUpper . head <*> tail) . drop ((length :: String -> Int) "resultLocationResponse_") }) ''ResultLocationResponse
#endif

data ResultResponse = ResultResponse
  { resultResponse_result :: [ResultLocationResponse]
  }
  deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
deriveJSON (defaultOptions { fieldLabelModifier = ((:) . toUpper . head <*> tail) . drop ((length :: String -> Int) "resultResponse_") }) ''ResultResponse
#endif

data ViewResponse = ViewResponse
  { viewResponse_view :: [ResultResponse]
  }
  deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
deriveJSON (defaultOptions { fieldLabelModifier = ((:) . toUpper . head <*> tail) . drop ((length :: String -> Int) "viewResponse_") }) ''ViewResponse
#endif

data RawResponse a = RawResponse
  { rawResponse_response :: a
  }
  deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
deriveJSON (defaultOptions { fieldLabelModifier = ((:) . toUpper . head <*> tail) . drop ((length :: String -> Int) "rawResponse_") }) ''RawResponse
#endif

#ifdef BACKEND
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
          route <- safeHead $ routeResponse_route $ rawRouteResponse_response response
          summary <- routeRouteResponse_summary route
          return $ routeRouteSummaryResponse_distance summary
      return $! result
  where
    safeHead [] = Nothing
    safeHead (a:_) = Just a

-- | Geocode an address into a coordinate
geocodeReq :: Manager -> HereMapsCredentials -> Text -> Text -> Text -> IO (Maybe (Double, Double))
geocodeReq mgr creds city state country = do
  tstart <- getCurrentTime
  let url = "https://geocoder.api.here.com/6.2/geocode.json?city=" <> T.unpack city <> "&state=" <> T.unpack state <> "&country=" <> T.unpack country <> hereMapsCredentialsQueryString creds --TODO: Encode URI components
  req <- parseUrlThrow url
  resp <- try $ responseBody <$> httpLbs (req { requestHeaders = ("Connection", "close") : requestHeaders req }) mgr
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
#endif

hereMapsCredentialsQueryString :: HereMapsCredentials -> String
hereMapsCredentialsQueryString creds = "&app_id=" ++ unpack (_hereMapsCredentials_appId creds) ++ "&app_code=" ++ unpack (_hereMapsCredentials_appCode creds)

#ifndef USE_TEMPLATE_HASKELL
--src/Focus/HereMaps.hs:38:1-30: Splicing declarations
--makeJson ''HereMapsCredentials
--  ======>
instance ToJSON HereMapsCredentials where
  toJSON r_a3s4c
    = case r_a3s4c of {
        HereMapsCredentials f_a3s4d f_a3s4e
          -> toJSON
               ("HereMapsCredentials" :: String,
              toJSON (HCons f_a3s4d (HCons f_a3s4e HNil))) }
instance FromJSON HereMapsCredentials where
  parseJSON v_a3s4f
    = do { (tag'_a3s4p, v'_a3s4q) <- parseJSON v_a3s4f;
           case tag'_a3s4p :: String of
             "HereMapsCredentials"
               -> do { HCons f_a3s4r (HCons f_a3s4s HNil) <- parseJSON v'_a3s4q;
                       return (HereMapsCredentials f_a3s4r f_a3s4s) }
             _ -> fail "invalid message" }
--src/Focus/HereMaps.hs:49:1-88: Splicing declarations
--deriveJSON
--  (defaultOptions {fieldLabelModifier = drop 26})
--  ''RouteRouteSummaryResponse
--  ======>
instance ToJSON RouteRouteSummaryResponse where
  toJSON
    = \ value_a3sCM
        -> case value_a3sCM of {
             RouteRouteSummaryResponse arg1_a3sCS
               -> object [keyValuePairWith toJSON (pack "distance") arg1_a3sCS] }
  toEncoding
    = \ value_a3sCU
        -> case value_a3sCU of {
             RouteRouteSummaryResponse arg1_a3sCV
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "distance"))
                            >< (colon >< (toEncoding arg1_a3sCV)))])) }
instance FromJSON RouteRouteSummaryResponse where
  parseJSON
    = \ value_a3sD3
        -> case value_a3sD3 of
             Object recObj_a3sD5
               -> (RouteRouteSummaryResponse
                   <$>
                     (lookupField
                        parseJSON
                        "Focus.HereMaps.RouteRouteSummaryResponse"
                        "RouteRouteSummaryResponse"
                        recObj_a3sD5
                        (pack "distance")))
             other_a3sDc
               -> parseTypeMismatch'
                    "RouteRouteSummaryResponse"
                    "Focus.HereMaps.RouteRouteSummaryResponse"
                    "Object"
                    (valueConName other_a3sDc)
--src/Focus/HereMaps.hs:59:1-81: Splicing declarations
--deriveJSON
--  (defaultOptions {fieldLabelModifier = drop 19})
--  ''RouteRouteResponse
--  ======>
instance ToJSON RouteRouteResponse where
  toJSON
    = \ value_a3tzN
        -> case value_a3tzN of {
             RouteRouteResponse arg1_a3tAG
               -> object [keyValuePairWith toJSON (pack "summary") arg1_a3tAG] }
  toEncoding
    = \ value_a3tAN
        -> case value_a3tAN of {
             RouteRouteResponse arg1_a3tAP
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "summary"))
                            >< (colon >< (toEncoding arg1_a3tAP)))])) }
instance FromJSON RouteRouteResponse where
  parseJSON
    = \ value_a3tDC
        -> case value_a3tDC of
             Object recObj_a3tDE
               -> (RouteRouteResponse
                   <$>
                     (lookupField
                        parseJSON
                        "Focus.HereMaps.RouteRouteResponse"
                        "RouteRouteResponse"
                        recObj_a3tDE
                        (pack "summary")))
             other_a3tDP
               -> parseTypeMismatch'
                    "RouteRouteResponse"
                    "Focus.HereMaps.RouteRouteResponse"
                    "Object"
                    (valueConName other_a3tDP)
--src/Focus/HereMaps.hs:69:1-76: Splicing declarations
--deriveJSON
--  (defaultOptions {fieldLabelModifier = drop 14}) ''RouteResponse
--  ======>
instance ToJSON RouteResponse where
  toJSON
    = \ value_a3ubS
        -> case value_a3ubS of {
             RouteResponse arg1_a3ucd
               -> object [keyValuePairWith toJSON (pack "route") arg1_a3ucd] }
  toEncoding
    = \ value_a3uci
        -> case value_a3uci of {
             RouteResponse arg1_a3ucj
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "route")) >< (colon >< (toEncoding arg1_a3ucj)))])) }
instance FromJSON RouteResponse where
  parseJSON
    = \ value_a3ucO
        -> case value_a3ucO of
             Object recObj_a3ucQ
               -> (RouteResponse
                   <$>
                     (lookupField
                        parseJSON
                        "Focus.HereMaps.RouteResponse"
                        "RouteResponse"
                        recObj_a3ucQ
                        (pack "route")))
             other_a3udT
               -> parseTypeMismatch'
                    "RouteResponse"
                    "Focus.HereMaps.RouteResponse"
                    "Object"
                    (valueConName other_a3udT)
--src/Focus/HereMaps.hs:78:1-124: Splicing declarations
--deriveJSON
--  (defaultOptions
--     {fieldLabelModifier = drop
--                             ((length :: String -> Int) "rawRouteResponse_")})
--  ''RawRouteResponse
--  ======>
instance ToJSON RawRouteResponse where
  toJSON
    = \ value_a3uNX
        -> case value_a3uNX of {
             RawRouteResponse arg1_a3uOb
               -> object [keyValuePairWith toJSON (pack "response") arg1_a3uOb] }
  toEncoding
    = \ value_a3uQO
        -> case value_a3uQO of {
             RawRouteResponse arg1_a3uQP
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "response"))
                            >< (colon >< (toEncoding arg1_a3uQP)))])) }
instance FromJSON RawRouteResponse where
  parseJSON
    = \ value_a3uR4
        -> case value_a3uR4 of
             Object recObj_a3uR5
               -> (RawRouteResponse
                   <$>
                     (lookupField
                        parseJSON
                        "Focus.HereMaps.RawRouteResponse"
                        "RawRouteResponse"
                        recObj_a3uR5
                        (pack "response")))
             other_a3uRc
               -> parseTypeMismatch'
                    "RawRouteResponse"
                    "Focus.HereMaps.RawRouteResponse"
                    "Object"
                    (valueConName other_a3uRc)
--src/Focus/HereMaps.hs:89:1-184: Splicing declarations
--deriveJSON
--  (defaultOptions
--     {fieldLabelModifier = ((:) . toUpper . head <*> tail)
--                           . drop
--                               ((length :: String -> Int) "displayPositionCoordsResponse_")})
--  ''DisplayPositionCoordsResponse
--  ======>
instance ToJSON DisplayPositionCoordsResponse where
  toJSON
    = \ value_a3vw8
        -> case value_a3vw8 of {
             DisplayPositionCoordsResponse arg1_a3vwC arg2_a3vwD
               -> object
                    [keyValuePairWith toJSON (pack "Latitude") arg1_a3vwC,
                     keyValuePairWith toJSON (pack "Longitude") arg2_a3vwD] }
  toEncoding
    = \ value_a3vwF
        -> case value_a3vwF of {
             DisplayPositionCoordsResponse arg1_a3vwG arg2_a3vwH
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "Latitude")) >< (colon >< (toEncoding arg1_a3vwG))),
                           ((text (pack "Longitude"))
                            >< (colon >< (toEncoding arg2_a3vwH)))])) }
instance FromJSON DisplayPositionCoordsResponse where
  parseJSON
    = \ value_a3vxb
        -> case value_a3vxb of
             Object recObj_a3vxc
               -> ((DisplayPositionCoordsResponse
                    <$>
                      (lookupField
                         parseJSON
                         "Focus.HereMaps.DisplayPositionCoordsResponse"
                         "DisplayPositionCoordsResponse"
                         recObj_a3vxc
                         (pack "Latitude")))
                   <*>
                     (lookupField
                        parseJSON
                        "Focus.HereMaps.DisplayPositionCoordsResponse"
                        "DisplayPositionCoordsResponse"
                        recObj_a3vxc
                        (pack "Longitude")))
             other_a3vxx
               -> parseTypeMismatch'
                    "DisplayPositionCoordsResponse"
                    "Focus.HereMaps.DisplayPositionCoordsResponse"
                    "Object"
                    (valueConName other_a3vxx)
--src/Focus/HereMaps.hs:99:1-158: Splicing declarations
--deriveJSON
--  (defaultOptions
--     {fieldLabelModifier = ((:) . toUpper . head <*> tail)
--                           . drop ((length :: String -> Int) "locationResponse_")})
--  ''LocationResponse
--  ======>
instance ToJSON LocationResponse where
  toJSON
    = \ value_a3wfS
        -> case value_a3wfS of {
             LocationResponse arg1_a3wg4
               -> object
                    [keyValuePairWith toJSON (pack "DisplayPosition") arg1_a3wg4] }
  toEncoding
    = \ value_a3wg7
        -> case value_a3wg7 of {
             LocationResponse arg1_a3wg9
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "DisplayPosition"))
                            >< (colon >< (toEncoding arg1_a3wg9)))])) }
instance FromJSON LocationResponse where
  parseJSON
    = \ value_a3wgt
        -> case value_a3wgt of
             Object recObj_a3wgv
               -> (LocationResponse
                   <$>
                     (lookupField
                        parseJSON
                        "Focus.HereMaps.LocationResponse"
                        "LocationResponse"
                        recObj_a3wgv
                        (pack "DisplayPosition")))
             other_a3wgH
               -> parseTypeMismatch'
                    "LocationResponse"
                    "Focus.HereMaps.LocationResponse"
                    "Object"
                    (valueConName other_a3wgH)
--src/Focus/HereMaps.hs:109:1-170: Splicing declarations
--deriveJSON
--  (defaultOptions
--     {fieldLabelModifier = ((:) . toUpper . head <*> tail)
--                           . drop ((length :: String -> Int) "resultLocationResponse_")})
--  ''ResultLocationResponse
--  ======>
instance ToJSON ResultLocationResponse where
  toJSON
    = \ value_a3wPg
        -> case value_a3wPg of {
             ResultLocationResponse arg1_a3wPk
               -> object [keyValuePairWith toJSON (pack "Location") arg1_a3wPk] }
  toEncoding
    = \ value_a3wPq
        -> case value_a3wPq of {
             ResultLocationResponse arg1_a3wPs
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "Location"))
                            >< (colon >< (toEncoding arg1_a3wPs)))])) }
instance FromJSON ResultLocationResponse where
  parseJSON
    = \ value_a3wPu
        -> case value_a3wPu of
             Object recObj_a3wPv
               -> (ResultLocationResponse
                   <$>
                     (lookupField
                        parseJSON
                        "Focus.HereMaps.ResultLocationResponse"
                        "ResultLocationResponse"
                        recObj_a3wPv
                        (pack "Location")))
             other_a3wPy
               -> parseTypeMismatch'
                    "ResultLocationResponse"
                    "Focus.HereMaps.ResultLocationResponse"
                    "Object"
                    (valueConName other_a3wPy)
--src/Focus/HereMaps.hs:118:1-154: Splicing declarations
--deriveJSON
--  (defaultOptions
--     {fieldLabelModifier = ((:) . toUpper . head <*> tail)
--                           . drop ((length :: String -> Int) "resultResponse_")})
--  ''ResultResponse
--  ======>
instance ToJSON ResultResponse where
  toJSON
    = \ value_a3xm6
        -> case value_a3xm6 of {
             ResultResponse arg1_a3xmg
               -> object [keyValuePairWith toJSON (pack "Result") arg1_a3xmg] }
  toEncoding
    = \ value_a3xmp
        -> case value_a3xmp of {
             ResultResponse arg1_a3xmr
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "Result"))
                            >< (colon >< (toEncoding arg1_a3xmr)))])) }
instance FromJSON ResultResponse where
  parseJSON
    = \ value_a3xmz
        -> case value_a3xmz of
             Object recObj_a3xmC
               -> (ResultResponse
                   <$>
                     (lookupField
                        parseJSON
                        "Focus.HereMaps.ResultResponse"
                        "ResultResponse"
                        recObj_a3xmC
                        (pack "Result")))
             other_a3xmJ
               -> parseTypeMismatch'
                    "ResultResponse"
                    "Focus.HereMaps.ResultResponse"
                    "Object"
                    (valueConName other_a3xmJ)
--src/Focus/HereMaps.hs:127:1-150: Splicing declarations
--deriveJSON
--  (defaultOptions
--     {fieldLabelModifier = ((:) . toUpper . head <*> tail)
--                           . drop ((length :: String -> Int) "viewResponse_")})
--  ''ViewResponse
--  ======>
instance ToJSON ViewResponse where
  toJSON
    = \ value_a3xXS
        -> case value_a3xXS of {
             ViewResponse arg1_a3xY1
               -> object [keyValuePairWith toJSON (pack "View") arg1_a3xY1] }
  toEncoding
    = \ value_a3xY5
        -> case value_a3xY5 of {
             ViewResponse arg1_a3xY6
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "View")) >< (colon >< (toEncoding arg1_a3xY6)))])) }
instance FromJSON ViewResponse where
  parseJSON
    = \ value_a3xYb
        -> case value_a3xYb of
             Object recObj_a3xYc
               -> (ViewResponse
                   <$>
                     (lookupField
                        parseJSON
                        "Focus.HereMaps.ViewResponse"
                        "ViewResponse"
                        recObj_a3xYc
                        (pack "View")))
             other_a3xYh
               -> parseTypeMismatch'
                    "ViewResponse"
                    "Focus.HereMaps.ViewResponse"
                    "Object"
                    (valueConName other_a3xYh)
--src/Focus/HereMaps.hs:136:1-148: Splicing declarations
--deriveJSON
--  (defaultOptions
--     {fieldLabelModifier = ((:) . toUpper . head <*> tail)
--                           . drop ((length :: String -> Int) "rawResponse_")})
--  ''RawResponse
--  ======>
instance ToJSON a_a3xZz => ToJSON (RawResponse a_a3xZz) where
  toJSON
    = \ value_a3yzC
        -> case value_a3yzC of {
             RawResponse arg1_a3yzG
               -> object [keyValuePairWith toJSON (pack "Response") arg1_a3yzG] }
  toEncoding
    = \ value_a3yzI
        -> case value_a3yzI of {
             RawResponse arg1_a3yzJ
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "Response"))
                            >< (colon >< (toEncoding arg1_a3yzJ)))])) }
instance FromJSON a_a3xZz => FromJSON (RawResponse a_a3xZz) where
  parseJSON
    = \ value_a3yzP
        -> case value_a3yzP of
             Object recObj_a3yzQ
               -> (RawResponse
                   <$>
                     (lookupField
                        parseJSON
                        "Focus.HereMaps.RawResponse"
                        "RawResponse"
                        recObj_a3yzQ
                        (pack "Response")))
             other_a3yzV
               -> parseTypeMismatch'
                    "RawResponse"
                    "Focus.HereMaps.RawResponse"
                    "Object"
                    (valueConName other_a3yzV)
#endif

-}
