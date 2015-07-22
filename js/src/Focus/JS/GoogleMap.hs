{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, TemplateHaskell, NoMonomorphismRestriction, EmptyDataDecls, RankNTypes, GADTs, RecursiveDo, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveDataTypeable, GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, UndecidableInstances #-}
module Focus.JS.GoogleMap where

import Control.Monad.IO.Class
import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.Writer hiding (forM, forM_, mapM, mapM_, sequence, (<>), listen)
import Control.Applicative
import GHCJS.DOM.Geolocation
import GHCJS.DOM.Types hiding (Event)
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens hiding (coerce)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Semigroup hiding (option)
import Data.Traversable
import Data.These
import Data.Align
import Data.Typeable

import Reflex
import Reflex.Dom

import Focus.JS.Request

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

JS(getGeolocation_, "navigator.geolocation", IO (JSRef Geolocation))
getGeolocation :: IO (Maybe (JSRef Geolocation))
getGeolocation = validJSRef <$> getGeolocation_
data GeolocationPosition
JS(geolocationGetCurrentPosition_, "$1.getCurrentPosition($2)", JSRef Geolocation -> JSFun (JSRef GeolocationPosition -> IO()) -> IO ())
geolocationGetCurrentPosition :: JSRef Geolocation -> (JSRef GeolocationPosition -> IO()) -> IO ()
geolocationGetCurrentPosition geo f = do
  rec cb <- syncCallback1 AlwaysRetain True $ \gp -> do
        f gp
        release cb
  geolocationGetCurrentPosition_ geo cb

geolocationPositionGetCoord :: JSRef GeolocationPosition -> IO (Double, Double)
geolocationPositionGetCoord p = do
  coords <- getProp "coords" p
  lat <- getProp "latitude" coords
  long <- getProp "longitude" coords
  Just lat' <- fromJSRef lat
  Just long' <- fromJSRef long
  return $ (lat', long')

data MapMarkerInfo
  = MapMarkerInfo { _mapMarkerInfo_coord :: (Double, Double)
                  , _mapMarkerInfo_title :: String
                  , _mapMarkerInfo_icon :: String
                  , _mapMarkerInfo_zIndex :: Double
                  }
    deriving (Typeable, Show, Read, Eq, Ord)

--deriving instance Typeable Any

googleMap :: forall t m a k. (MonadWidget t m, Ord k, Show k) => Dynamic t (Map k MapMarkerInfo) -> m ()
googleMap dTargetMarkers = do
  (e,_) :: (El t, ()) <- elAttr' "div" ("style" =: "height:713px") $ return ()
  m <- liftIO $ newGoogleMap $ _el_element e
  _ <- liftIO $ googleMapTriggerResize m
  let dMyGoogleMap = constDyn $ Just m
  let updateMarkers :: Maybe (GoogleMap, (Map k MapMarkerInfo, Map k GoogleMapMarker)) -> WidgetHost m (Map k GoogleMapMarker, Any)
      updateMarkers = maybe (return (Map.empty, mempty)) $ \(gm, (target, current)) -> runWriterT $ liftM (Map.mapMaybe id) $ iforM (align target current) $ \k tc -> case tc of
        This t -> do
          tell $ Any True
          liftM Just $ liftIO $ googleMapAddMarker gm (_mapMarkerInfo_coord t) (_mapMarkerInfo_title t) (_mapMarkerInfo_icon t) (_mapMarkerInfo_zIndex t)
        These t c -> do
          liftIO $ googleMapMarkerSetCoord c (_mapMarkerInfo_coord t)
          liftIO $ googleMapMarkerSetTitle c (_mapMarkerInfo_title t)
          liftIO $ googleMapMarkerSetIcon c (_mapMarkerInfo_icon t)
          liftIO $ googleMapMarkerSetZIndex c (_mapMarkerInfo_zIndex t)
          return $ Just c
        That c -> do
          tell $ Any True
          liftIO $ googleMapMarkerRemove c
          return Nothing
  rec dCurrentMarkers <- holdDyn Map.empty . (fmapMaybe (\(x, Any b) -> if b then Just x else Nothing)) =<< performEvent . (fmap updateMarkers) . updated =<< combineDyn (\gm x -> fmap (flip (,) x) gm) dMyGoogleMap =<< combineDyn (,) dTargetMarkers dCurrentMarkers
  return ()

newtype GoogleMap = GoogleMap { unGoogleMap :: JSRef GoogleMap } deriving (Typeable)
JS(newGoogleMap_, "new google.maps.Map($1, {center: new google.maps.LatLng(43.7, -79.4), zoom: 12, mapTypeId: google.maps.MapTypeId.ROADMAP})", JSRef Element -> IO (JSRef GoogleMap))
newGoogleMap :: IsElement e => e -> IO GoogleMap
newGoogleMap e = do
  m <- newGoogleMap_ (unElement (toElement e))
  return $ GoogleMap m
JS(googleMapTriggerResize_, "google.maps.event.trigger($1, 'resize')", JSRef GoogleMap -> IO ())
googleMapTriggerResize :: GoogleMap -> IO ()
googleMapTriggerResize = googleMapTriggerResize_ . unGoogleMap

newtype GoogleMapLatLng = GoogleMapLatLng { unGoogleMapLatLng :: JSRef GoogleMapLatLng }
JS(googleMapGetCenter_, "$1.getCenter()", JSRef GoogleMap -> IO (JSRef GoogleMapLatLng))
googleMapGetCenter :: GoogleMap -> IO GoogleMapLatLng
googleMapGetCenter m = do
  c <- googleMapGetCenter_ (unGoogleMap m)
  return $ GoogleMapLatLng c
JS(googleMapSetCenter_, "$1.setCenter($2)", JSRef GoogleMap -> JSRef GoogleMapLatLng -> IO ())
googleMapSetCenter :: GoogleMap -> GoogleMapLatLng -> IO ()
googleMapSetCenter m c = do
  googleMapSetCenter_ (unGoogleMap m) (unGoogleMapLatLng c)
  return ()

newtype GoogleMapMarker = GoogleMapMarker { unGoogleMapMarker :: JSRef GoogleMapMarker } deriving (Typeable)
JS(googleMapAddMarker_, "new google.maps.Marker({position: new google.maps.LatLng($2, $3), map: $1, title: $4, icon: $5, zIndex: $6})", JSRef GoogleMap -> JSRef Double -> JSRef Double -> JSRef String -> JSRef String -> JSRef Double -> IO (JSRef GoogleMapMarker))
JS(googleMapMarkerSetZIndex_, "$1.setZIndex($2)", JSRef GoogleMapMarker -> JSRef Double -> IO ())
googleMapMarkerSetZIndex :: GoogleMapMarker -> Double -> IO ()
googleMapMarkerSetZIndex m z = do
  zIndex <- toJSRef z
  googleMapMarkerSetZIndex_ (unGoogleMapMarker m) zIndex
googleMapAddMarker :: GoogleMap -> (Double, Double) -> String -> String -> Double -> IO GoogleMapMarker
googleMapAddMarker m c t i z = do
  lat <- toJSRef $ fst c
  long <- toJSRef $ snd c
  title <- toJSRef t
  icon <- toJSRef i
  zIndex <- toJSRef z
  marker <- googleMapAddMarker_ (unGoogleMap m) lat long title icon zIndex
  return $ GoogleMapMarker marker
JS(googleMapMarkerRemove_, "$1.setMap(null)", JSRef GoogleMapMarker -> IO ())
googleMapMarkerRemove :: GoogleMapMarker -> IO ()
googleMapMarkerRemove = googleMapMarkerRemove_ . unGoogleMapMarker
JS(googleMapMarkerSetIcon_, "$1.setIcon($2)", JSRef GoogleMapMarker -> JSRef String -> IO ())
googleMapMarkerSetIcon :: GoogleMapMarker -> String -> IO ()
googleMapMarkerSetIcon mm icon = do
  i <- toJSRef icon
  googleMapMarkerSetIcon_ (unGoogleMapMarker mm) i
JS(googleMapMarkerSetTitle_, "$1.setTitle($2)", JSRef GoogleMapMarker -> JSRef String -> IO ())
googleMapMarkerSetTitle :: GoogleMapMarker -> String -> IO ()
googleMapMarkerSetTitle mm t = do
  i <- toJSRef t
  googleMapMarkerSetTitle_ (unGoogleMapMarker mm) i
JS(googleMapMarkerSetCoord_, "$1.setPosition(new google.maps.LatLng($2, $3))", JSRef GoogleMapMarker -> JSRef Double -> JSRef Double -> IO ())
googleMapMarkerSetCoord :: GoogleMapMarker -> (Double, Double) -> IO ()
googleMapMarkerSetCoord mm c = do
  lat <- toJSRef $ fst c
  long <- toJSRef $ snd c
  googleMapMarkerSetCoord_ (unGoogleMapMarker mm) lat long

JS(googleMapFitToBounds_, "$1.fitBounds(new google.maps.LatLngBounds(new google.maps.LatLng($2, $3), new google.maps.LatLng($4, $5)))", JSRef GoogleMap -> JSRef Double -> JSRef Double -> JSRef Double -> JSRef Double -> IO ())
googleMapFitToCoords :: GoogleMap -> [(Double, Double)] -> IO ()
googleMapFitToCoords m coords = case nonEmpty coords of
  Nothing -> return ()
  Just l ->
    let (Min minLat, Min minLng, Max maxLat, Max maxLng) = sconcat $ fmap (\(lat', lng') -> (Min lat', Min lng', Max lat', Max lng')) l
    in do
      minLatRef <- toJSRef minLat
      minLngRef <- toJSRef minLng
      maxLatRef <- toJSRef maxLat
      maxLngRef <- toJSRef maxLng
      googleMapFitToBounds_ (unGoogleMap m) minLatRef minLngRef maxLatRef maxLngRef

newtype GoogleMapPolyline = GoogleMapPolyline { unGoogleMapPolyline :: JSRef GoogleMapPolyline }
JS(googleMapPolyline_, "new google.maps.Polyline({path: [new google.maps.LatLng($2, $3), new google.maps.LatLng($4, $5)], strokeOpacity: 0, icons: [{ icon: { path: 'M 0,-1 0,1', strokeOpacity: 0.6, scale: 4 }, offset: '0', repeat: '20px'}], map: $1})", JSRef GoogleMap -> JSRef Double -> JSRef Double -> JSRef Double -> JSRef Double -> IO (JSRef GoogleMapPolyline))
googleMapAddPolyline :: GoogleMap -> (Double, Double) -> (Double, Double) -> IO GoogleMapPolyline
googleMapAddPolyline m c1 c2 = do
  a <- toJSRef $ fst c1
  b <- toJSRef $ snd c1
  c <- toJSRef $ fst c2
  d <- toJSRef $ snd c2
  p <- googleMapPolyline_ (unGoogleMap m) a b c d
  return $ GoogleMapPolyline p

JS(googleMapPolylineRemove_, "$1.setMap(null)", JSRef GoogleMapPolyline -> IO ())
googleMapPolylineRemove :: GoogleMapPolyline -> IO ()
googleMapPolylineRemove = googleMapPolylineRemove_ . unGoogleMapPolyline
JS(googleMapPolylineSetPath_, "$1.setPath([new google.maps.LatLng($2, $3), new google.maps.LatLng($4, $5)])", JSRef GoogleMapPolyline -> JSRef Double -> JSRef Double -> JSRef Double -> JSRef Double -> IO ())
googleMapPolylineSetPath :: GoogleMapPolyline -> (Double, Double) -> (Double, Double) -> IO ()
googleMapPolylineSetPath p c1 c2 = do
  a <- toJSRef $ fst c1
  b <- toJSRef $ snd c1
  c <- toJSRef $ fst c2
  d <- toJSRef $ snd c2
  googleMapPolylineSetPath_ (unGoogleMapPolyline p) a b c d

newtype GeocoderPlace = GeocoderPlace { unGeocoderPlace :: JSRef GeocoderPlace } deriving (Typeable)
JS(googleMapsGeocoderPlace_, "new google.maps.Geocoder().geocode({ address: $1 }, $2)", JSRef String -> JSFun (JSArray GeocoderPlace -> JSRef () -> IO ()) -> IO ())

googleMapsGeocoderPlace :: String -> ([(String, (Double, Double))] -> IO ()) -> IO ()
googleMapsGeocoderPlace s f =  do
  rec cb <- syncCallback2 AlwaysRetain True $ \result status -> if isNull result then return () else do
        a <- fromArray result
        a' <- forM a $ \p -> do
          Just formatted_address <- fromJSRef =<< getProp "formatted_address" p
          locationRef <- getProp "location" =<< getProp "geometry" p
          Just lat <- fromJSRef =<< placeDetailsLat_ locationRef
          Just lng <- fromJSRef =<< placeDetailsLng_ locationRef
          return $ (formatted_address, (lat, lng))
        f a'
        release cb
  s' <- toJSRef s
  googleMapsGeocoderPlace_ s' cb

newtype PlacesAutocompleteService = PlacesAutocompleteService { unPlacesAutocompleteService :: JSRef PlacesAutocompleteService } deriving Typeable
JS(googleMapsPlacesAutocompleteService_, "new google.maps.places.AutocompleteService(null, {})", IO (JSRef PlacesAutocompleteService))

newtype PlacesAutocompletePrediction = PlacesAutocompletePrediction { unPlacesAutocompletePrediction :: JSRef PlacesAutocompletePrediction } deriving Typeable
JS(googleMapsPlacesGetPlacePredictions_, "$1.getPlacePredictions({input: $2}, $3)", JSRef PlacesAutocompleteService -> JSRef String -> JSFun (JSArray PlacesAutocompletePrediction -> JSRef String -> IO ()) -> IO ())

newtype PlacesAutocompletePredictionReference = PlacesAutocompletePredictionReference { unPlacesAutocompletePredictionReference :: JSRef PlacesAutocompletePredictionReference } deriving Typeable

newtype PlaceDetails = PlaceDetails { unPlaceDetails :: JSRef PlaceDetails } deriving Typeable
JS(googleMapsPlacesServiceGetDetails_, "new google.maps.places.PlacesService(document.createElement('div')).getDetails({placeId: $1},  $2)", JSRef PlacesAutocompletePredictionReference -> JSFun (JSRef PlaceDetails -> JSRef String -> IO ()) -> IO ())

googleMapsAutocompletePlace :: String -> ([(String, PlacesAutocompletePredictionReference)] -> IO ()) -> IO ()
googleMapsAutocompletePlace s f =  do
  rec cb <- syncCallback2 AlwaysRetain True $ \result status -> if isNull result then return () else do
        a <- fromArray result
        a' <- forM a $ \p -> do
          Just description <- fromJSRef =<< getProp "description" p
          reference <- getProp "place_id" p
          return $ (description, PlacesAutocompletePredictionReference reference)
        f a'
        release cb
  s' <- toJSRef s
  g <- googleMapsPlacesAutocompleteService_
  googleMapsPlacesGetPlacePredictions_ g s' cb

JS(placeDetailsLat_, "$1.lat()", JSRef PlaceDetails -> IO (JSRef Double))
JS(placeDetailsLng_, "$1.lng()", JSRef PlaceDetails -> IO (JSRef Double))

googleMapsAutocompletePlaceDetails :: PlacesAutocompletePredictionReference -> ((Double, Double)-> IO ()) -> IO ()
googleMapsAutocompletePlaceDetails ref f = do
  rec cb <- syncCallback2 AlwaysRetain True $ \result status -> if isNull result then return () else do
        locationRef <- getProp "location" =<< getProp "geometry" result
        Just lat <- fromJSRef =<< placeDetailsLat_ locationRef
        Just lng <- fromJSRef =<< placeDetailsLng_ locationRef
        f (lat, lng)
        release cb
  let ref' = unPlacesAutocompletePredictionReference ref
  googleMapsPlacesServiceGetDetails_ ref' cb

geocodeSearch :: forall t m a. MonadWidget t m => String -> String -> Maybe (String, (Double, Double)) -> Event t (Maybe (String, (Double, Double))) -> Dynamic t (Map String String) -> m (Dynamic t (Maybe (String, (Double, Double))))
geocodeSearch googleLogoPath googleLogoClass l0 setL inputAttrs = do
  --TODO: Rate limiting
  --TODO: Don't display choices inline
  --TODO: Deal with the situation where results are returned in the wrong order
  let geoResults results = searchInputResultsList' results $ \r -> do
        l <- list r searchInputResult
        elAttr "img" (Map.fromList [("src", googleLogoPath), ("class", googleLogoClass)]) $ return () --https://developers.google.com/places/policies#logo_requirements
        return l
  rec (eInputChanged, eChoice) <- searchInput' (maybe "" fst l0) (fmap (maybe "" $ fst) setL) inputAttrs eResults geoResults
      eResults :: Event t (Map Integer (String, PlacesAutocompletePredictionReference)) <- liftM (fmap (Map.fromList . zip [(1::Integer)..])) $ performEventAsync $ fmap (\s -> liftIO . googleMapsAutocompletePlace s) $ fmapMaybe id $ fmap (\i -> if i == "" then Nothing else Just i) eInputChanged
      eLocation <- getPlaceDetails eChoice
  holdDyn l0 $ leftmost [fmap Just eLocation, setL]

getPlaceDetails :: MonadWidget t m => Event t (String, PlacesAutocompletePredictionReference) -> m (Event t (String, (Double, Double)))
getPlaceDetails eChoice = performEventAsync $ fmap (\(address, ref) cb -> liftIO $ googleMapsAutocompletePlaceDetails ref $ \result -> cb (address, result)) eChoice

searchInputResult :: forall t m a. MonadWidget t m => Dynamic t (String, a) -> m (Event t (String, a))
searchInputResult r = el "li" $ do
  (li, _) <- elAttr' "a" (Map.singleton "style" "cursor: pointer;") $ dynText =<< mapDyn fst r
  return $ tag (current r) (_el_clicked li)

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

