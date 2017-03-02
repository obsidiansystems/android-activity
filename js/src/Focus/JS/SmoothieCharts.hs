{-# LANGUAGE CPP, ForeignFunctionInterface, JavaScriptFFI, OverloadedStrings, TemplateHaskell, RankNTypes, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
module Focus.JS.SmoothieCharts where
--http://smoothiecharts.org/

#if 0

import Reflex
import Reflex.Dom.Core

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Char
import Data.Default
import qualified Data.Text as T
import Data.Text.Encoding
import GHCJS.DOM.Types hiding (Event, Text)
import qualified Data.ByteString.Lazy as LBS

newtype SmoothieChart x = SmoothieChart { unSmoothieChart :: JSRef x }

instance ToJS x (SmoothieChart x) where
  withJS (SmoothieChart r) = ($ r)

instance FromJS x (SmoothieChart x) where
  fromJS = return . SmoothieChart

newtype TimeSeries x = TimeSeries { unTimeSeries :: JSRef x }

instance ToJS x (TimeSeries x) where
  withJS (TimeSeries r) = ($ r)

instance FromJS x (TimeSeries x) where
  fromJS = return . TimeSeries

newtype TimeSeriesOptions x = TimeSeriesOptions { unTimeSeriesOptions :: JSRef x }

instance ToJS x (TimeSeriesOptions x) where
  withJS (TimeSeriesOptions r) = ($ r)

instance FromJS x (TimeSeriesOptions x) where
  fromJS = return . TimeSeriesOptions

data Interpolation = Bezier
                   | Linear
                   | Step
                   deriving (Show, Read, Eq, Ord)

data SmoothieTimeSeriesStyle
   = SmoothieTimeSeriesStyle { _smoothieTimeSeriesStyle_lineWidth :: Double
                             , _smoothieTimeSeriesStyle_strokeStyle :: String
                             , _smoothieTimeSeriesStyle_fillStyle :: Maybe String
                             } deriving (Show, Read, Eq, Ord)

data SmoothieTimeSeriesConfig
   = SmoothieTimeSeriesConfig { _smoothieTimeSeriesConfig_resetBounds :: Bool
                              , _smoothieTimeSeriesConfig_resetBoundsInterval :: Int
                              } deriving (Show, Read, Eq, Ord)

instance Default SmoothieTimeSeriesConfig where
  def = SmoothieTimeSeriesConfig True 3000

instance Default SmoothieTimeSeriesStyle where
  def = SmoothieTimeSeriesStyle 1 "#ffffff" Nothing

data SmoothieChartConfig
   = SmoothieChartConfig { _smoothieChartConfig_maxValueScale :: Double
                         , _smoothieChartConfig_minValueScale :: Double
                         , _smoothieChartConfig_millisPerPixel :: Double
                         , _smoothieChartConfig_grid :: SmoothieChartGridConfig
                         , _smoothieChartConfig_labels :: SmoothieChartLabelConfig
                         , _smoothieChartConfig_maxValue :: Maybe Double
                         , _smoothieChartConfig_minValue :: Maybe Double
                         , _smoothieChartConfig_enableDpiScaling :: Bool
                         , _smoothieChartConfig_scaleSmoothing :: Double
                         , _smoothieChartConfig_maxDataSetLength :: Int
                         , _smoothieChartConfig_interpolation :: Interpolation
                         , _smoothieChartConfig_scrollBackwards :: Bool
                         } deriving (Show, Read, Eq, Ord)

instance Default SmoothieChartConfig where
  def = SmoothieChartConfig { _smoothieChartConfig_maxValueScale = 1
                            , _smoothieChartConfig_minValueScale = 1
                            , _smoothieChartConfig_millisPerPixel = 20
                            , _smoothieChartConfig_grid = def
                            , _smoothieChartConfig_labels = def
                            , _smoothieChartConfig_maxValue = Nothing
                            , _smoothieChartConfig_minValue = Nothing
                            , _smoothieChartConfig_enableDpiScaling = True
                            , _smoothieChartConfig_scaleSmoothing = 0.125
                            , _smoothieChartConfig_maxDataSetLength = 2
                            , _smoothieChartConfig_interpolation = Bezier
                            , _smoothieChartConfig_scrollBackwards = False
                            }

data SmoothieChartGridConfig
   = SmoothieChartGridConfig { _smoothieChartGridConfig_fillStyle :: String
                             , _smoothieChartGridConfig_lineWidth :: Double
                             , _smoothieChartGridConfig_strokeStyle :: String
                             , _smoothieChartGridConfig_millisPerLine :: Double
                             , _smoothieChartGridConfig_sharpLines :: Bool
                             , _smoothieChartGridConfig_verticalSections :: Int
                             , _smoothieChartGridConfig_borderVisible :: Bool
                             } deriving (Show, Read, Eq, Ord)

instance Default SmoothieChartGridConfig where
  def = SmoothieChartGridConfig { _smoothieChartGridConfig_fillStyle = "#000000"
                                , _smoothieChartGridConfig_lineWidth = 1
                                , _smoothieChartGridConfig_strokeStyle = "#777777"
                                , _smoothieChartGridConfig_millisPerLine = 1000
                                , _smoothieChartGridConfig_sharpLines = False
                                , _smoothieChartGridConfig_verticalSections = 2
                                , _smoothieChartGridConfig_borderVisible = True
                                }

data SmoothieChartLabelConfig
   = SmoothieChartLabelConfig { _smoothieChartLabelConfig_disabled :: Bool
                              , _smoothieChartLabelConfig_fillStyle :: String
                              , _smoothieChartLabelConfig_fontSize :: Double
                              , _smoothieChartLabelConfig_fontFamily :: String
                              , _smoothieChartLabelConfig_precision :: Double
                              } deriving (Show, Read, Eq, Ord)

instance Default SmoothieChartLabelConfig where
  def = SmoothieChartLabelConfig { _smoothieChartLabelConfig_disabled = False
                                 , _smoothieChartLabelConfig_fillStyle = "#ffffff"
                                 , _smoothieChartLabelConfig_fontSize = 15
                                 , _smoothieChartLabelConfig_fontFamily = "sans-serif"
                                 , _smoothieChartLabelConfig_precision = 2
                                 }

deriveJSON (defaultOptions { constructorTagModifier = map toLower }) ''Interpolation
deriveJSON (defaultOptions { fieldLabelModifier = drop (length ("_smoothieTimeSeriesConfig_" :: String)), omitNothingFields = True }) ''SmoothieTimeSeriesConfig
deriveJSON (defaultOptions { fieldLabelModifier = drop (length ("_smoothieChartGridConfig_" :: String)), omitNothingFields = True }) ''SmoothieChartGridConfig
deriveJSON (defaultOptions { fieldLabelModifier = drop (length ("_smoothieChartLabelConfig_" :: String)), omitNothingFields = True }) ''SmoothieChartLabelConfig
deriveJSON (defaultOptions { fieldLabelModifier = drop (length ("_smoothieTimeSeriesStyle_" :: String)), omitNothingFields = True }) ''SmoothieTimeSeriesStyle
deriveJSON (defaultOptions { fieldLabelModifier = drop (length ("_smoothieChartConfig_" :: String)), omitNothingFields = True }) ''SmoothieChartConfig

liftM concat $ mapM makeLenses
  [ ''SmoothieChartConfig
  , ''SmoothieChartLabelConfig
  , ''SmoothieChartGridConfig
  , ''SmoothieTimeSeriesStyle
  , ''SmoothieTimeSeriesConfig
  ]

importJS Unsafe "new SmoothieChart(JSON['parse'](this[0]))" "smoothieChartNew_" [t| forall x m. MonadJS x m => String -> m (SmoothieChart x) |]
importJS Unsafe "this[0]['streamTo'](this[1], this[2])" "smoothieStreamTo_" [t| forall x m. MonadJS x m => SmoothieChart x -> Node -> Double -> m () |]
importJS Unsafe "new TimeSeries(JSON['parse'](this[0]))" "smoothieTimeSeriesNew_" [t| forall x m. MonadJS x m => String -> m (TimeSeries x) |]
importJS Unsafe "this[0]['addTimeSeries'](this[1], JSON['parse'](this[2]))" "smoothieAddTimeSeries_" [t| forall x m. MonadJS x m => SmoothieChart x -> TimeSeries x -> String -> m () |]
importJS Unsafe "this[0]['append'](this[1], this[2])" "smoothieTimeSeriesAppend_" [t| forall x m. MonadJS x m => TimeSeries x -> Double -> Double -> m () |]
importJS Unsafe "new Date()['getTime']()" "smoothieTimestamp_" [t| forall x m. MonadJS x m => m Double |]
importJS Unsafe "this[0]['getTimeSeriesOptions'](this[1])" "smoothieGetTimeSeriesOptions_" [t| forall x m. MonadJS x m => SmoothieChart x -> TimeSeries x -> m (TimeSeriesOptions x) |]
importJS Unsafe "(function(that){for (var key in that[0]) { that[0][key] = JSON['parse'](that[1])[key] }})(this)" "smoothieSetTimeSeriesOptions_" [t| forall x m. MonadJS x m => TimeSeriesOptions x -> String -> m () |]

smoothieChartNew :: MonadJS x m => SmoothieChartConfig -> m (SmoothieChart x)
smoothieChartNew = smoothieChartNew_ . encodeToJsonString

--smoothieStreamTo :: MonadJS x m => (SmoothieChart x) -> El t -> Double -> m ()
--smoothieStreamTo s e lag = smoothieStreamTo_ s (toNode $ _el_element e) lag

smoothieTimeSeriesNew :: MonadJS x m => SmoothieTimeSeriesConfig -> m (TimeSeries x)
smoothieTimeSeriesNew = smoothieTimeSeriesNew_ . encodeToJsonString

smoothieAddTimeSeries :: MonadJS x m => SmoothieChart x -> TimeSeries x -> SmoothieTimeSeriesStyle -> m ()
smoothieAddTimeSeries s t = smoothieAddTimeSeries_ s t . encodeToJsonString

smoothieTimeSeriesAppend :: MonadJS x m => TimeSeries x -> Double -> Double -> m ()
smoothieTimeSeriesAppend ts t x = smoothieTimeSeriesAppend_ ts t x

smoothieTimeSeriesAppendWithCurrentTime :: MonadJS x m => TimeSeries x -> Double -> m ()
smoothieTimeSeriesAppendWithCurrentTime ts x = do
  t <- smoothieTimestamp_
  smoothieTimeSeriesAppend ts t x

{-
smoothieChart :: (HasJS x m, HasJS x (WidgetHost m), MonadWidget t m) => Map String String -> Double -> [(Event t Double, SmoothieTimeSeriesConfig, SmoothieTimeSeriesStyle)] -> SmoothieChartConfig -> m (SmoothieChart x, [TimeSeries x])
smoothieChart attrs lag es cfg = do
  (canvas, _) <- elAttr' "canvas" attrs $ return ()
  (ets, s) <- do
    s <- liftJS $ smoothieChartNew cfg
    ets <- forM es $ \(e, cfg', style) -> liftJS $ do
      ts <- smoothieTimeSeriesNew cfg'
      smoothieAddTimeSeries s ts style
      return (e, ts)
    liftJS $ smoothieStreamTo s canvas lag
    return (ets, s)
  forM_ ets $ \(e, ts) -> performEvent_ $ fmap (\c -> liftJS $ smoothieTimeSeriesAppendWithCurrentTime ts c) e
  return $ (s, map snd ets)
-}

smoothieChartSetTimeSeriesStyle :: MonadJS x m => SmoothieChart x -> TimeSeries x -> SmoothieTimeSeriesStyle -> m ()
smoothieChartSetTimeSeriesStyle sc t cfg = do
  tso <- smoothieGetTimeSeriesOptions_ sc t
  withFS <- case _smoothieTimeSeriesStyle_fillStyle cfg of
    Nothing -> do
      u <- mkJSUndefined
      return ($ u)
    Just fs' -> return $ withJS fs'
  withFS $ \fs -> setJSProp "fillStyle" fs $ unTimeSeriesOptions tso
  withJS (_smoothieTimeSeriesStyle_strokeStyle cfg) $ \ss -> setJSProp "strokeStyle" ss $ unTimeSeriesOptions tso
  withJS (_smoothieTimeSeriesStyle_lineWidth cfg) $ \lw -> setJSProp "lineWidth" lw $ unTimeSeriesOptions tso

updateTimeSeriesStyle :: (HasJS x (WidgetHost m), MonadWidget t m) => SmoothieChart x -> TimeSeries x -> Event t SmoothieTimeSeriesStyle -> m ()
updateTimeSeriesStyle sc ts e = performEvent_ $ fmap (liftJS . smoothieChartSetTimeSeriesStyle sc ts) e

encodeToJsonString :: ToJSON a => a -> String
encodeToJsonString = T.unpack . decodeUtf8 . LBS.toStrict . encode

#endif
