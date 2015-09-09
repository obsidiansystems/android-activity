{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, TemplateHaskell, NoMonomorphismRestriction, EmptyDataDecls, RankNTypes, GADTs, RecursiveDo, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveDataTypeable, GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, UndecidableInstances #-}
module Focus.JS.GoogleMap where

import Control.Lens hiding (coerce, zoom)
import Control.Monad.IO.Class
import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.Writer hiding (forM, forM_, mapM, mapM_, sequence, (<>), listen)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Align
import Data.Bifunctor
import Data.Char (isSpace)
import Data.Default
import Data.Either
import Data.List.NonEmpty (nonEmpty)
import Data.Semigroup hiding (option)
import Data.Traversable
import Data.These
import Data.Typeable
import Data.Maybe

import Focus.JS.FontAwesome (icon)
import Focus.Schema (ShowPretty)
import Reflex
import Reflex.Dom

import Foreign.JavaScript.TH
import GHCJS.DOM.Types hiding (Event)

newtype GeolocationPosition x = GeolocationPosition { unGeolocationPosition :: JSRef x }

instance ToJS x (GeolocationPosition x) where
  withJS (GeolocationPosition x) = ($ x)

instance FromJS x (GeolocationPosition x) where
  fromJS = return . GeolocationPosition

data MapMarkerInfo
  = MapMarkerInfo { _mapMarkerInfo_coord :: (Double, Double)
                  , _mapMarkerInfo_title :: String
                  , _mapMarkerInfo_icon :: String
                  , _mapMarkerInfo_zIndex :: Double
                  }
    deriving (Typeable, Show, Read, Eq, Ord)

newtype GoogleMap x = GoogleMap { unGoogleMap :: JSRef x }

instance ToJS x (GoogleMap x) where
  withJS (GoogleMap x) = ($ x)

instance FromJS x (GoogleMap x) where
  fromJS = return . GoogleMap

newtype GoogleMapMarker x = GoogleMapMarker { unGoogleMapMarker :: JSRef x }

instance ToJS x (GoogleMapMarker x) where
  withJS (GoogleMapMarker x) = ($ x)

instance FromJS x (GoogleMapMarker x) where
  fromJS = return . GoogleMapMarker

newtype PlacesAutocompletePredictionReference x = PlacesAutocompletePredictionReference { unPlacesAutocompletePredictionReference :: JSRef x }

instance ToJS x (PlacesAutocompletePredictionReference x) where
  withJS (PlacesAutocompletePredictionReference x) = ($ x)

instance FromJS x (PlacesAutocompletePredictionReference x) where
  fromJS = return . PlacesAutocompletePredictionReference

newtype PlaceDetails x = PlaceDetails { unPlaceDetails :: JSRef x }

instance ToJS x (PlaceDetails x) where
  withJS (PlaceDetails x) = ($ x)

instance FromJS x (PlaceDetails x) where
  fromJS = return . PlaceDetails

newtype PlacesAutocompleteService x = PlacesAutocompleteService { unPlacesAutocompleteService :: JSRef x }

instance ToJS x (PlacesAutocompleteService x) where
  withJS (PlacesAutocompleteService x) = ($ x)

instance FromJS x (PlacesAutocompleteService x) where
  fromJS = return . PlacesAutocompleteService

newtype PlacesAutocompletePrediction x = PlacesAutocompletePrediction { unPlacesAutocompletePrediction :: JSRef x }

instance ToJS x (PlacesAutocompletePrediction x) where
  withJS (PlacesAutocompletePrediction x) = ($ x)

instance FromJS x (PlacesAutocompletePrediction x) where
  fromJS = return . PlacesAutocompletePrediction

importJS Unsafe "navigator['geolocation']['getCurrentPosition'](this[0])" "getGeolocationCurrentPosition_" [t| forall x m. MonadJS x m => JSFun x -> m () |]
importJS Unsafe "new google['maps']['Map'](this[0], {center: new google['maps']['LatLng'](this[1], this[2]), zoom: this[3], mapTypeId: google['maps']['MapTypeId']['ROADMAP']})" "newGoogleMap" [t| forall x m. MonadJS x m => Node -> Double -> Double -> Double -> m (GoogleMap x) |]

importJS Unsafe "google['maps']['event']['trigger'](this[0], 'resize')" "googleMapTriggerResize" [t| forall x m. MonadJS x m => GoogleMap x -> m () |]

importJS Unsafe "this[0]['fitBounds'](new google['maps']['LatLngBounds'](new google['maps']['LatLng'](this[1], this[2]), new google['maps']['LatLng'](this[3], this[4])))" "googleMapFitToBounds_" [t| forall x m. MonadJS x m => GoogleMap x -> Double -> Double -> Double -> Double -> m() |]

importJS Unsafe "this[0]['setCenter'](new google['maps']['LatLng'](this[1], this[2]))" "googleMapSetCenter" [t| forall x m. MonadJS x m => GoogleMap x -> Double -> Double -> m () |]

importJS Unsafe "this[0]['setZoom'](this[1])" "googleMapSetZoom" [t| forall x m. MonadJS x m => GoogleMap x -> Double -> m () |]

importJS Unsafe "new google['maps']['Marker']({position: new google['maps']['LatLng'](this[1], this[2]), map: this[0], title: this[3], icon: this[4], zIndex: this[5]})" "googleMapAddMarker_" [t| forall x m. MonadJS x m => GoogleMap x -> Double -> Double -> String -> String -> Double -> m (GoogleMapMarker x) |]

googleMapAddMarker :: MonadJS x m => GoogleMap x -> (Double, Double) -> String -> String -> Double -> m (GoogleMapMarker x)
googleMapAddMarker g c = googleMapAddMarker_ g (fst c) (snd c)

importJS Unsafe "this[0]['setZIndex'](this[1])" "googleMapMarkerSetZIndex" [t| forall x m. MonadJS x m => GoogleMapMarker x -> Double -> m () |]

importJS Unsafe "this[0]['setTitle'](this[1])" "googleMapMarkerSetTitle" [t| forall x m. MonadJS x m => GoogleMapMarker x -> String -> m () |]

importJS Unsafe "this[0]['setIcon'](this[1])" "googleMapMarkerSetIcon" [t| forall x m. MonadJS x m => GoogleMapMarker x -> String -> m () |]

importJS Unsafe "this[0]['setPosition'](new google['maps']['LatLng'](this[1], this[2]))" "googleMapMarkerSetCoord_" [t| forall x m. MonadJS x m => GoogleMapMarker x -> Double -> Double -> m () |]

googleMapMarkerSetCoord :: MonadJS x m => GoogleMapMarker x -> (Double, Double) -> m ()
googleMapMarkerSetCoord g c = googleMapMarkerSetCoord_ g (fst c) (snd c)

importJS Unsafe "this[0]['setMap'](null)" "googleMapMarkerRemove" [t| forall x m. MonadJS x m => GoogleMapMarker x -> m () |]

importJS Unsafe "(function(that) { var pid = {}; pid['placeId'] = that[0]; new google['maps']['places']['PlacesService'](document['createElement']('div'))['getDetails'](pid, that[1]) })(this)" "googleMapsPlacesServiceGetDetails_" [t| forall x m. MonadJS x m => PlacesAutocompletePredictionReference x -> JSFun x -> m () |] --(JSRef PlaceDetails -> JSRef String -> IO ()) -> m () |]

importJS Unsafe "this[0]['lat']()" "placeDetailsLat_" [t| forall x m. MonadJS x m => PlaceDetails x -> m Double |]
importJS Unsafe "this[0]['lng']()" "placeDetailsLng_" [t| forall x m. MonadJS x m => PlaceDetails x -> m Double |]

importJS Unsafe "new google['maps']['places']['AutocompleteService'](null, {})" "googleMapsPlacesAutocompleteService_" [t| forall x m. MonadJS x m => m (PlacesAutocompleteService x) |]

importJS Unsafe "this[0]['getPlacePredictions']({input: this[1]}, this[2])" "googleMapsPlacesGetPlacePredictions_" [t| forall x m. MonadJS x m => PlacesAutocompleteService x -> String -> JSFun x -> m () |]
-- (JSArray PlacesAutocompletePrediction -> JSRef String -> IO ()) -> IO ())

getGeolocationCurrentPosition :: (MonadJS x m, MonadFix m, MonadIO m) => (GeolocationPosition x -> IO ()) -> m ()
getGeolocationCurrentPosition cb = do
  rec f <- mkJSFun $ \(success:_) -> do
             s <- fromJS success
             liftIO $ cb s
             freeJSFun f
             mkJSUndefined
  getGeolocationCurrentPosition_ f

geolocationPositionGetCoord :: (MonadJS x m) => GeolocationPosition x -> m (Double, Double)
geolocationPositionGetCoord pos = do
  withJS pos $ \p -> do
    coords <- getJSProp "coords" p
    lat <- fromJSNumber =<< getJSProp "latitude" coords
    long <- fromJSNumber =<< getJSProp "longitude" coords
    return (lat, long)

googleMapFitToCoords :: (MonadJS x m) => GoogleMap x -> [(Double, Double)] -> m ()
googleMapFitToCoords m coords = case coordsToBounds coords of
  Nothing -> return ()
  Just ((minLat, minLng), (maxLat, maxLng)) -> googleMapFitToBounds_ m minLat minLng maxLat maxLng

coordsToBounds :: [(Double, Double)] -> Maybe ((Double, Double), (Double, Double))
coordsToBounds coords = case nonEmpty coords of
  Nothing -> Nothing
  Just l -> let (Min minLat, Min minLng, Max maxLat, Max maxLng) = sconcat $ fmap (\(lat', lng') -> (Min lat', Min lng', Max lat', Max lng')) l
            in Just $ ((minLat, minLng), (maxLat, maxLng))

data WebMapConfig t
   = WebMapConfig { _webMapConfig_initialCenter :: (Double, Double)
                  , _webMapConfig_initialZoom :: Double
                  , _webMapConfig_attributes :: Dynamic t (Map String String)
                  , _webMapConfig_triggerResize :: Event t ()
                  , _webMapConfig_fitToCoords :: Event t [(Double, Double)]
                  , _webMapConfig_setCenter :: Event t (Double, Double)
                  , _webMapConfig_setZoom :: Event t Double
                  }

instance Reflex t => Default (WebMapConfig t) where
  def = WebMapConfig { _webMapConfig_initialCenter = (0, 0)
                     , _webMapConfig_initialZoom = 1
                     , _webMapConfig_attributes = constDyn mempty
                     , _webMapConfig_triggerResize = never
                     , _webMapConfig_fitToCoords = never
                     , _webMapConfig_setCenter = never
                     , _webMapConfig_setZoom = never
                     }

data WebMap t x
   = WebMap { _webMap_object :: GoogleMap x
            , _webMap_element :: El t
            }

googleMap :: forall t m k x. (HasJS x m, HasJS x (WidgetHost m), MonadWidget t m, Ord k, Show k) => Dynamic t (Map k MapMarkerInfo) -> WebMapConfig t -> m (WebMap t x)
googleMap dTargetMarkers (WebMapConfig (lat0, lng0) zoom0 attrs resize fitToCoords center zoom) = do
  (e,_) :: (El t, ()) <- elDynAttr' "div" attrs $ return ()
  m <- liftJS $ newGoogleMap (toNode $ _el_element e) lat0 lng0 zoom0
  _ <- liftJS $ googleMapTriggerResize m
  let dMyGoogleMap = constDyn $ Just m
  let updateMarkers :: Maybe (GoogleMap x, (Map k MapMarkerInfo, Map k (GoogleMapMarker x))) -> WidgetHost m (Map k (GoogleMapMarker x), Any)
      updateMarkers = maybe (return (Map.empty, mempty)) $ \(gm, (target, present)) -> runWriterT $ liftM (Map.mapMaybe id) $ iforM (align target present) $ \_ tc -> case tc of
        This t -> do
          tell $ Any True
          liftM Just $ lift $ liftJS $ googleMapAddMarker gm (_mapMarkerInfo_coord t) (_mapMarkerInfo_title t) (_mapMarkerInfo_icon t) (_mapMarkerInfo_zIndex t)
        These t c -> do
          lift $ liftJS $ googleMapMarkerSetCoord c (_mapMarkerInfo_coord t)
          lift $ liftJS $ googleMapMarkerSetTitle c (_mapMarkerInfo_title t)
          lift $ liftJS $ googleMapMarkerSetIcon c (_mapMarkerInfo_icon t)
          lift $ liftJS $ googleMapMarkerSetZIndex c (_mapMarkerInfo_zIndex t)
          return $ Just c
        That c -> do
          tell $ Any True
          lift $ liftJS $ googleMapMarkerRemove c
          return Nothing
  rec dCurrentMarkers <- holdDyn Map.empty . (fmapMaybe (\(x, Any b) -> if b then Just x else Nothing)) =<< performEvent . (fmap updateMarkers) . updated =<< combineDyn (\gm x -> fmap (flip (,) x) gm) dMyGoogleMap =<< combineDyn (,) dTargetMarkers dCurrentMarkers
  performEvent_ $ fmap (liftJS . googleMapFitToCoords m) fitToCoords
  performEvent_ $ fmap (liftJS . uncurry (googleMapSetCenter m)) center
  performEvent_ $ fmap (liftJS . googleMapSetZoom m) zoom
  performEvent_ $ fmap (\_ -> liftJS $ googleMapTriggerResize m) resize
  return $ WebMap m e

searchInputResult :: forall t m a. MonadWidget t m => Dynamic t (String, a) -> m (Event t (String, a))
searchInputResult r = el "li" $ do
  (li, _) <- elAttr' "a" (Map.singleton "style" "cursor: pointer;") $ dynText =<< mapDyn fst r
  return $ tag (current r) (domEvent Click li)

-- TODO: A lot of this stuff seems like it belongs either in Focus.JS.Bootstrap or Focus.JS.Widget as it has little to do with Google Maps
searchInput :: forall t m a k. (MonadWidget t m, Ord k) => Dynamic t (Map String String) -> Event t (Map k (String, a)) -> m (Event t String, Event t (String, a))
searchInput attrs results = searchInput' "" never attrs results searchInputResultsList

searchInput' :: forall t m a k. (MonadWidget t m, Ord k) => String -> Event t String -> Dynamic t (Map String String) -> Event t (Map k (String, a)) -> (Dynamic t (Map k (String, a)) -> m (Event t (String, a))) -> m (Event t String, Event t (String, a))
searchInput' v0 setV attrs results listBuilder = do
  rec input <- textInput $ def & textInputConfig_setValue .~ eSetValue
                               & attributes .~ attrs
                               & textInputConfig_initialValue .~ v0
      let enter = textInputGetEnter input
      dResults <- holdDyn mempty $ leftmost [eClearResults, results]
      eMadeChoice <- listBuilder dResults
      let eSetValue = leftmost [fmap fst eMadeChoice, setV]
          eSelectionMade = fmap (const Nothing) eSetValue
          eInputChanged = fmapMaybe id $ leftmost [eSelectionMade, fmap Just (updated $ _textInput_value input), fmap Just (tag (current $ value input) enter)]
          eInputEmpty = fmapMaybe id $ fmap (\i -> if i == "" then Just Map.empty else Nothing) eInputChanged
          eClearResults = leftmost [eInputEmpty, fmap (const Map.empty) eMadeChoice]
  return (eInputChanged, eMadeChoice)

searchInputResultsList :: forall t m a k. (MonadWidget t m, Ord k) => Dynamic t (Map k (String, a)) -> m (Event t (String, a))
searchInputResultsList results = searchInputResultsList' results (flip list searchInputResult)

searchInputResultsList' :: forall t m a k. (MonadWidget t m, Ord k) => Dynamic t (Map k (String, a)) -> (Dynamic t (Map k (String, a)) -> m (Dynamic t (Map k (Event t (String, a))))) -> m (Event t (String, a))
searchInputResultsList' results builder = do
  let hideDropdown = Map.fromList [("class", "dropdown-menu"), ("style", "display: none;")]
      showDropdown = Map.fromList [("class", "dropdown-menu"), ("style", "display: block;")]
  attrs <- mapDyn (\rs -> if Map.null rs then hideDropdown else showDropdown) results
  resultsList <- elDynAttr "ul" attrs $ builder results
  liftM switch $ hold never $ fmap (leftmost . Map.elems) (updated resultsList)

twoSourceSearch
    :: forall t x m a b. (HasJS x m, HasJS x (WidgetHost m), MonadWidget t m)
    => String
    -> String
    -> String
    -> (Event t (Int, String) -> m (Event t (Int, [(String, a)])))
    -> (Event t (Int, String) -> m (Event t (Int, [(String, b)])))
    -> m (Dynamic t (Maybe (Either a b)))
twoSourceSearch placeholder al bl searchAs searchBs = do
  rec (queryChange, selection) <- divClass "input-group" $ do
        elClass "span" "input-group-addon" $ icon "globe"
        searchInput' "" never (constDyn $ "class" =: "form-control" <> "placeholder" =: placeholder) results (resultsList al bl)
      counter :: Dynamic t Int <- foldDyn (+) 0 $ fmap (const 1) queryChange
      let taggedQuery = attachWith (,) (current counter) queryChange
      aResults' :: Event t (Int, [(String, a)]) <- searchAs taggedQuery
      bResults' :: Event t (Int, [(String, b)]) <- searchBs taggedQuery
      as <- holdDyn (0, []) aResults'
      bs <- holdDyn (0, []) bResults'
      resultsReady <- combineDyn (\(t1, a) (t2, b) -> if t1 == t2 then Just (alignResults (These a b)) else Nothing) as bs
      let results = fmapMaybe id $ updated resultsReady
  holdDyn Nothing $ fmap (Just . snd) selection
  where
    alignResults :: These [(String, a)] [(String, b)] -> (Map Int (String, Either a b))
    alignResults results' =
      let toMap c xs = Map.fromList $ map (\(k, (s, v)) -> (k, (s, c v))) $ zip [1::Int ..] xs
      in case results' of
           This as -> toMap Left as
           That bs -> toMap Right bs
           These as bs -> Map.fromList $ zip [1::Int ..] (map (second Left) as ++ map (second Right) bs)
    resultsList aLabel bLabel results = do
      let open = "class" =: "dropdown-menu" <> "style" =: "display: block;"
          closed = "class" =: "dropdown-menu" <> "style" =: "display: none;"
      attrs <- forDyn results $ \r -> if Map.null r then closed else open
      (as, bs) <- splitDyn =<< forDyn results (\rs -> Map.partition (isLeft . snd) rs)
      elDynAttr "ul" attrs $ do
        elClass "li" "dropdown-header" $ text aLabel
        dyn =<< mapDyn (\x -> if Map.null x then elClass "li" "disabled" (elClass "a" "disabled-pointer" (text "No results")) else return ()) as
        as' <- list as searchInputResult
        elClass "li" "dropdown-header" $ text bLabel
        dyn =<< mapDyn (\x -> if Map.null x then elClass "li" "disabled" (elClass "a" "disabled-pointer" (text "No results")) else return ()) bs
        bs' <- list bs searchInputResult
        results' <- combineDyn Map.union as' bs'
        liftM switch $ hold never $ fmap (leftmost . Map.elems) (updated results')

googleMapsAutocompletePlaceDetails :: (MonadFix m, MonadJS x m, MonadIO m) => PlacesAutocompletePredictionReference x -> (((Double, Double), Map AddressComponent AddressComponentValue) -> IO ()) -> m ()
googleMapsAutocompletePlaceDetails ref cb = do
  rec jsCb <- mkJSFun $ \(result:_) -> do
                n <- isJSNull result
                case n of
                     True -> mkJSUndefined
                     False -> do
                       locationRef <- fromJS =<< getJSProp "location" =<< getJSProp "geometry" result
                       acRef <- fromJSArray =<< getJSProp "address_components" result
                       addressComponents <- liftM toAddressComponents $ forM acRef $ \ac -> do
                         longName <- fromJSString =<< getJSProp "long_name" ac
                         shortName <- fromJSString =<< getJSProp "short_name" ac
                         typesRef <- fromJSArray =<< getJSProp "types" ac
                         types <- forM typesRef fromJSString
                         return (longName, shortName, types)
                       liftIO $ print addressComponents
                       lat <- placeDetailsLat_ locationRef
                       lng <- placeDetailsLng_ locationRef
                       liftIO $ cb ((lat, lng), addressComponents)
                       freeJSFun jsCb
                       mkJSUndefined
  googleMapsPlacesServiceGetDetails_ ref jsCb

data AddressComponentValue
   = AddressComponentValue { _addressComponentValue_longName :: String
                           , _addressComponentValue_shortName :: String
                           }
                           deriving (Show, Read, Eq, Ord)

-- See https://developers.google.com/maps/documentation/geocoding/intro#Types
data AddressComponent = AddressComponent_StreetNumber
                      | AddressComponent_Route
                      | AddressComponent_Neighborhood
                      | AddressComponent_AdministrativeAreaLevel1 -- administrative_area_level_1 indicates a first-order civil entity
                                                                  -- below the country level. Within the United States,
                                                                  -- these administrative levels are states.
                      | AddressComponent_AdministrativeAreaLevel2 -- administrative_area_level_2 indicates a second-order civil entity
                                                                  -- below the country level. Within the United States, these administrative
                                                                  -- levels are counties
                      | AddressComponent_Locality -- locality indicates an incorporated city or town political entity.
                      | AddressComponent_Sublocality
                      | AddressComponent_PostalCode
                      | AddressComponent_Country
                      deriving (Show, Read, Eq, Ord, Enum, Bounded)

toAddressComponents :: [(String, String, [String])] -> Map AddressComponent AddressComponentValue
toAddressComponents acs = Map.fromList $ catMaybes $ concatMap (\(long, short, ts) -> map (f long short) ts) acs
  where
    toAc t = case t of
                  "street_number" -> Just AddressComponent_StreetNumber
                  "route" -> Just AddressComponent_Route
                  "neighborhood" -> Just AddressComponent_Neighborhood
                  "administrative_area_level_1" -> Just AddressComponent_AdministrativeAreaLevel1
                  "administrative_area_level_2" -> Just AddressComponent_AdministrativeAreaLevel2
                  "locality" -> Just AddressComponent_Locality
                  "sublocality" -> Just AddressComponent_Sublocality
                  "postal_code" -> Just AddressComponent_PostalCode
                  "country" -> Just AddressComponent_Country
                  _ -> Nothing
    f l s t = fmap (\ac' -> (ac', AddressComponentValue l s)) $ toAc t


googleMapsAutocompletePlace' :: (MonadJS x m, MonadFix m, MonadIO m) => String -> ([(String, PlacesAutocompletePredictionReference x)] -> IO ()) -> m ()
googleMapsAutocompletePlace' s cb =  do
  rec jsCb <- mkJSFun $ \(result:_) -> do
        a <- fromJSArray result
        a' <- forM a $ \p -> do
                desc <- fromJSString =<< getJSProp "description" p
                ref <- getJSProp "place_id" p
                return (desc, PlacesAutocompletePredictionReference ref)
        liftIO $ cb a'
        freeJSFun jsCb
        mkJSUndefined
  g <- googleMapsPlacesAutocompleteService_
  googleMapsPlacesGetPlacePredictions_ g s jsCb

googleMapsAutocompletePlace :: forall x m t. (HasJS x m, HasJS x (WidgetHost m), MonadWidget t m)
                            => Event t (Int, String)                           -- ^ Queries tagged with index number
                            -> m (Event t (Int, [(String, PlacesAutocompletePredictionReference x)])) -- ^ Responses tagged with matching index number
googleMapsAutocompletePlace queryE = performEventAsync . fmap (\(n,s) -> performer n s) . ffilter (\(n,s) -> dropWhile isSpace s /= "") $ queryE
    where performer n s yield = liftJS . googleMapsAutocompletePlace' s $ \results -> yield (n, results)

geocodeSearch :: forall t m x. (HasJS x m, HasJS x (WidgetHost m), MonadWidget t m)
              => String                                           -- ^ Path to Google logo
              -> String                                           -- ^ CSS class of Google logo
              -> Maybe (String, (Double, Double))                 -- ^ Initial resulting location
              -> Event t (Maybe (String, (Double, Double)))       -- ^ Event which causes the selected location to be set
              -> Dynamic t (Map String String)                    -- ^ HTML attributes of input control
              -> m (Dynamic t (Maybe (String, (Double, Double)))) -- ^ Name and location selected
geocodeSearch googleLogoPath googleLogoClass l0 setL inputAttrs = do
  --TODO: Rate limiting
  --TODO: Deal with the situation where results are returned in the wrong order
  let geoResults results = searchInputResultsList' results $ \r -> do
        l <- list r searchInputResult
        elAttr "img" (Map.fromList [("src", googleLogoPath), ("class", googleLogoClass)]) $ return () --https://developers.google.com/places/policies#logo_requirements
        return l
  rec (eInputChanged, eChoice) <- searchInput' (maybe "" fst l0) (fmap (maybe "" $ fst) setL) inputAttrs eResults geoResults
      eResults :: Event t (Map Integer (String, PlacesAutocompletePredictionReference x)) <-
        liftM (fmap (Map.fromList . zip [(1::Integer)..]))
          . performEventAsync
          . fmap (\s -> liftJS . googleMapsAutocompletePlace' s)
          . ffilter (/= "")
          $ eInputChanged
      eLocation <- getPlaceDetails eChoice
  holdDyn l0 $ leftmost [fmap Just eLocation, setL]

getPlaceDetails :: (HasJS x m, HasJS x (WidgetHost m), MonadWidget t m) => Event t (String, PlacesAutocompletePredictionReference x) -> m (Event t (String, (Double, Double)))
getPlaceDetails eChoice = performEventAsync $ fmap (\(address, ref) cb -> liftJS $ googleMapsAutocompletePlaceDetails ref $ \result -> cb (address, fst result)) eChoice

getPlaceDetails' :: (HasJS x m, HasJS x (WidgetHost m), MonadWidget t m) => Event t (String, PlacesAutocompletePredictionReference x) -> m (Event t (String, ((Double, Double), Map AddressComponent AddressComponentValue)))
getPlaceDetails' eChoice = performEventAsync $ fmap (\(address, ref) cb -> liftJS $ googleMapsAutocompletePlaceDetails ref $ \result -> cb (address, result)) eChoice

-- newtype GoogleMapLatLng = GoogleMapLatLng { unGoogleMapLatLng :: JSRef GoogleMapLatLng }
-- JS(googleMapGetCenter_, "$1.getCenter()", JSRef GoogleMap -> IO (JSRef GoogleMapLatLng))
-- googleMapGetCenter :: GoogleMap -> IO GoogleMapLatLng
-- googleMapGetCenter m = do
--   c <- googleMapGetCenter_ (unGoogleMap m)
--   return $ GoogleMapLatLng c
-- 
-- newtype GoogleMapPolyline = GoogleMapPolyline { unGoogleMapPolyline :: JSRef GoogleMapPolyline }
-- JS(googleMapPolyline_, "new google.maps.Polyline({path: [new google.maps.LatLng($2, $3), new google.maps.LatLng($4, $5)], strokeOpacity: 0, icons: [{ icon: { path: 'M 0,-1 0,1', strokeOpacity: 0.6, scale: 4 }, offset: '0', repeat: '20px'}], map: $1})", JSRef GoogleMap -> JSRef Double -> JSRef Double -> JSRef Double -> JSRef Double -> IO (JSRef GoogleMapPolyline))
-- googleMapAddPolyline :: GoogleMap -> (Double, Double) -> (Double, Double) -> IO GoogleMapPolyline
-- googleMapAddPolyline m c1 c2 = do
--   a <- toJSRef $ fst c1
--   b <- toJSRef $ snd c1
--   c <- toJSRef $ fst c2
--   d <- toJSRef $ snd c2
--   p <- googleMapPolyline_ (unGoogleMap m) a b c d
--   return $ GoogleMapPolyline p
-- 
-- JS(googleMapPolylineRemove_, "$1.setMap(null)", JSRef GoogleMapPolyline -> IO ())
-- googleMapPolylineRemove :: GoogleMapPolyline -> IO ()
-- googleMapPolylineRemove = googleMapPolylineRemove_ . unGoogleMapPolyline
-- JS(googleMapPolylineSetPath_, "$1.setPath([new google.maps.LatLng($2, $3), new google.maps.LatLng($4, $5)])", JSRef GoogleMapPolyline -> JSRef Double -> JSRef Double -> JSRef Double -> JSRef Double -> IO ())
-- googleMapPolylineSetPath :: GoogleMapPolyline -> (Double, Double) -> (Double, Double) -> IO ()
-- googleMapPolylineSetPath p c1 c2 = do
--   a <- toJSRef $ fst c1
--   b <- toJSRef $ snd c1
--   c <- toJSRef $ fst c2
--   d <- toJSRef $ snd c2
--   googleMapPolylineSetPath_ (unGoogleMapPolyline p) a b c d
-- 
-- newtype GeocoderPlace = GeocoderPlace { unGeocoderPlace :: JSRef GeocoderPlace } deriving (Typeable)
-- JS(googleMapsGeocoderPlace_, "new google.maps.Geocoder().geocode({ address: $1 }, $2)", JSRef String -> JSFun (JSArray GeocoderPlace -> JSRef () -> IO ()) -> IO ())
-- 
-- googleMapsGeocoderPlace :: String -> ([(String, (Double, Double))] -> IO ()) -> IO ()
-- googleMapsGeocoderPlace s f =  do
--   rec cb <- syncCallback2 AlwaysRetain True $ \result status -> if isNull result then return () else do
--         a <- fromArray result
--         a' <- forM a $ \p -> do
--           Just formatted_address <- fromJSRef =<< getProp "formatted_address" p
--           locationRef <- getProp "location" =<< getProp "geometry" p
--           Just lat <- fromJSRef =<< placeDetailsLat_ locationRef
--           Just lng <- fromJSRef =<< placeDetailsLng_ locationRef
--           return $ (formatted_address, (lat, lng))
--         f a'
--         release cb
--   s' <- toJSRef s
--   googleMapsGeocoderPlace_ s' cb
