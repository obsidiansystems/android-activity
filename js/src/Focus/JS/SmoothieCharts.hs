{-# LANGUAGE CPP, ForeignFunctionInterface, JavaScriptFFI, OverloadedStrings, TemplateHaskell #-}
module Focus.JS.SmoothieCharts where
--http://smoothiecharts.org/

import Reflex
import Reflex.Dom
import Focus.JS.Request

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Bifunctor
import Data.Bitraversable
import Data.Char
import Data.Default
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHCJS.DOM.Types hiding (Event, Text)
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif


newtype SmoothieChart = SmoothieChart { unSmoothieChart :: JSRef SmoothieChart }
newtype TimeSeries = TimeSeries { unTimeSeries :: JSRef TimeSeries }
newtype TimeSeriesOptions = TimeSeriesOptions { unTimeSeriesOptions :: JSRef TimeSeriesOptions }

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

JS(smoothieChartNew_, "new SmoothieChart(JSON.parse($1))", JSString -> IO (JSRef SmoothieChart))
JS(smoothieStreamTo_, "$1.streamTo($2, $3)", JSRef SmoothieChart -> JSRef Element -> Double -> IO ())
JS(smoothieTimeSeriesNew_, "new TimeSeries(JSON.parse($1))", JSString -> IO (JSRef TimeSeries))
JS(smoothieAddTimeSeries_, "$1.addTimeSeries($2, JSON.parse($3))", JSRef SmoothieChart -> JSRef TimeSeries -> JSString -> IO ())
JS(smoothieTimeSeriesAppend_, "$1.append($2, $3)", JSRef TimeSeries -> Double -> Double -> IO ())
JS(smoothieTimestamp_, "new Date().getTime()", IO Double)
JS(smoothieGetTimeSeriesOptions_, "$1.getTimeSeriesOptions($2)", JSRef SmoothieChart -> JSRef TimeSeries -> IO (JSRef TimeSeriesOptions))
JS(smoothieSetTimeSeriesOptions_, "for (var key in $1) { $1[key] = JSON.parse($2)[key] }", JSRef TimeSeriesOptions -> JSString -> IO ())

smoothieChartNew :: SmoothieChartConfig -> IO SmoothieChart
smoothieChartNew cfg = do
  s <- smoothieChartNew_ $ encodeToJsonJSString cfg
  return $ SmoothieChart s

smoothieStreamTo :: SmoothieChart -> El t -> Double -> IO ()
smoothieStreamTo s e delay = smoothieStreamTo_ (unSmoothieChart s) (unElement $ toElement $ _el_element e) delay

smoothieTimeSeriesNew :: SmoothieTimeSeriesConfig -> IO TimeSeries
smoothieTimeSeriesNew cfg = liftM TimeSeries $ smoothieTimeSeriesNew_ (encodeToJsonJSString cfg)

smoothieAddTimeSeries :: SmoothieChart -> TimeSeries -> SmoothieTimeSeriesStyle -> IO ()
smoothieAddTimeSeries s t style = smoothieAddTimeSeries_ (unSmoothieChart s) (unTimeSeries t) (encodeToJsonJSString style)

smoothieTimeSeriesAppend :: TimeSeries -> Double -> Double -> IO ()
smoothieTimeSeriesAppend ts t x = smoothieTimeSeriesAppend_ (unTimeSeries ts) t x

smoothieTimeSeriesAppendWithCurrentTime :: TimeSeries -> Double -> IO ()
smoothieTimeSeriesAppendWithCurrentTime ts x = do
  t <- smoothieTimestamp_
  smoothieTimeSeriesAppend ts t x

smoothieChart :: MonadWidget t m => Map String String -> Double -> [(Event t Double, SmoothieTimeSeriesConfig, SmoothieTimeSeriesStyle)] -> SmoothieChartConfig -> m (SmoothieChart, [TimeSeries])
smoothieChart attrs delay es cfg = do
  (canvas, _) <- elAttr' "canvas" attrs $ return ()
  (ets, s) <- liftIO $ do
    s <- smoothieChartNew cfg
    ets <- forM es $ \(e, cfg', style) -> do
      ts <- smoothieTimeSeriesNew cfg'
      smoothieAddTimeSeries s ts style
      return (e, ts)
    smoothieStreamTo s canvas delay
    return (ets, s)
  forM ets $ \(e, ts) -> performEvent_ $ fmap (\c -> liftIO $ smoothieTimeSeriesAppendWithCurrentTime ts c) e
  return $ (s, map snd ets)

smoothieChartSetTimeSeriesStyle :: SmoothieChart -> TimeSeries -> SmoothieTimeSeriesStyle -> IO ()
smoothieChartSetTimeSeriesStyle sc t cfg = do
  tsc <- smoothieGetTimeSeriesOptions_ (unSmoothieChart sc) (unTimeSeries t)
  lw <- toJSRef (_smoothieTimeSeriesStyle_lineWidth cfg)
  ss <- toJSRef (_smoothieTimeSeriesStyle_strokeStyle cfg)
  fs <- case _smoothieTimeSeriesStyle_fillStyle cfg of
             Nothing -> return jsUndefined
             Just fs' -> toJSRef fs'
  setProp ("fillStyle" :: String) fs tsc
  setProp ("strokeStyle" :: String) ss tsc
  setProp ("lineWidth" :: String) lw tsc

updateTimeSeriesStyle :: MonadWidget t m => SmoothieChart -> TimeSeries -> Event t SmoothieTimeSeriesStyle -> m ()
updateTimeSeriesStyle sc ts e = performEvent_ $ fmap (liftIO . smoothieChartSetTimeSeriesStyle sc ts) e

encodeToJsonJSString :: ToJSON a => a -> JSString
encodeToJsonJSString = toJSString . decodeUtf8 . LBS.toStrict . encode
