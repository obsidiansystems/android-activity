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

instance ToJSON Interpolation where
  toJSON i = case i of
                  Bezier -> "bezier"
                  Linear -> "linear"
                  Step -> "step"

data SmoothieTimeSeriesConfig
   = SmoothieTimeSeriesConfig { _smoothieTimeSeriesConfig_lineWidth :: Double
                              , _smoothieTimeSeriesConfig_strokeStyle :: String
                              , _smoothieTimeSeriesConfig_fillStyle :: Maybe String
                              } deriving (Show, Read, Eq, Ord)

instance Default SmoothieTimeSeriesConfig where
  def = SmoothieTimeSeriesConfig 1 "#ffffff" Nothing

deriveJSON (defaultOptions { fieldLabelModifier = drop (length ("_smoothieTimeSeriesConfig_" :: String)) }) ''SmoothieTimeSeriesConfig

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

instance ToJSON SmoothieChartLabelConfig where
  toJSON (SmoothieChartLabelConfig disabled fillStyle fontSize fontFamily precision) =
    object [ "disabled" .= disabled
           , "fillStyle" .= fillStyle
           , "fontSize" .= fontSize
           , "fontFamily" .= fontFamily
           , "precision" .= precision
           ]

instance ToJSON SmoothieChartGridConfig where
  toJSON (SmoothieChartGridConfig fillStyle lineWidth strokeStyle millisPerLine sharpLines verticalSections borderVisible) =
    object [ "fillStyle" .= fillStyle
           , "lineWidth" .= lineWidth
           , "strokeStyle" .= strokeStyle
           , "millisPerLine" .= millisPerLine
           , "sharpLines" .= sharpLines
           , "verticalSections" .= verticalSections
           , "borderVisible" .= borderVisible
           ]

instance ToJSON SmoothieChartConfig where
  toJSON (SmoothieChartConfig maxValueScale minValueScale millisPerPixel grid labels maxValue minValue enableDpiScaling scaleSmoothing maxDataSetLength interpolation scrollBackwards) =
    object $ [ "maxValueScale" .= maxValueScale
             , "minValueScale" .= minValueScale
             , "millisPerPixel" .= millisPerPixel
             , "grid" .= grid
             , "labels" .= labels
             , "enableDpiScaling" .= enableDpiScaling
             , "scaleSmoothing" .= scaleSmoothing
             , "maxDataSetLength" .= maxDataSetLength
             , "interpolation" .= interpolation
             , "scrollBackwards" .= scrollBackwards
             ] ++ optionalFields [ ("maxValue", maxValue)
                                 , ("minValue", minValue)
                                 ]

optionalFields :: (ToJSON a) => [(Text, Maybe a)] -> [Pair]
optionalFields fs =
  let fs' = catMaybes $ map (bisequence . first Just) fs
  in map (uncurry (.=)) fs'

liftM concat $ mapM makeLenses
  [ ''SmoothieChartConfig
  , ''SmoothieChartLabelConfig
  , ''SmoothieChartGridConfig
  ]

JS(smoothieChartNew_, "new SmoothieChart(JSON.parse($1))", JSString -> IO (JSRef SmoothieChart))
JS(smoothieStreamTo_, "$1.streamTo($2, $3)", JSRef SmoothieChart -> JSRef Element -> Double -> IO ())
JS(smoothieTimeSeriesNew_, "new TimeSeries({resetBounds: false})", IO (JSRef TimeSeries))
JS(smoothieAddTimeSeries_, "$1.addTimeSeries($2, {lineWidth:2,strokeStyle:'#00ff00'})", JSRef SmoothieChart -> JSRef TimeSeries -> IO ())
JS(smoothieTimeSeriesAppend_, "$1.append($2, $3)", JSRef TimeSeries -> Double -> Double -> IO ())
JS(smoothieTimestamp_, "new Date().getTime()", IO Double)
JS(smoothieGetTimeSeriesOptions_, "$1.getTimeSeriesOptions($2)", JSRef SmoothieChart -> JSRef TimeSeries -> IO (JSRef TimeSeriesOptions))
JS(smoothieSetTimeSeriesOptions_, "for (var key in $1) { $1[key] = JSON.parse($2)[key] }", JSRef TimeSeriesOptions -> JSString -> IO ())

smoothieChartNew :: SmoothieChartConfig -> IO SmoothieChart
smoothieChartNew cfg = do
  s <- smoothieChartNew_ $ toJSString $ decodeUtf8 $ LBS.toStrict $ encode cfg
  return $ SmoothieChart s

smoothieStreamTo :: SmoothieChart -> El t -> Double -> IO ()
smoothieStreamTo s e delay = smoothieStreamTo_ (unSmoothieChart s) (unElement $ toElement $ _el_element e) delay

smoothieTimeSeriesNew :: IO TimeSeries
smoothieTimeSeriesNew = liftM TimeSeries smoothieTimeSeriesNew_

smoothieAddTimeSeries :: SmoothieChart -> TimeSeries -> IO ()
smoothieAddTimeSeries s t = smoothieAddTimeSeries_ (unSmoothieChart s) (unTimeSeries t)

smoothieTimeSeriesAppend :: TimeSeries -> Double -> Double -> IO ()
smoothieTimeSeriesAppend ts t x = smoothieTimeSeriesAppend_ (unTimeSeries ts) t x

smoothieTimeSeriesAppendWithCurrentTime :: TimeSeries -> Double -> IO ()
smoothieTimeSeriesAppendWithCurrentTime ts x = do
  t <- smoothieTimestamp_
  smoothieTimeSeriesAppend ts t x

smoothieChart :: MonadWidget t m => Map String String -> Double -> [Event t Double] -> SmoothieChartConfig -> m (SmoothieChart, [TimeSeries])
smoothieChart attrs delay es cfg = do
  (canvas, _) <- elAttr' "canvas" attrs $ return ()
  (ets, s) <- liftIO $ do
    s <- smoothieChartNew cfg
    ets <- forM es $ \e -> do
      ts <- smoothieTimeSeriesNew
      smoothieAddTimeSeries s ts
      return (e, ts)
    smoothieStreamTo s canvas delay
    return (ets, s)
  forM ets $ \(e, ts) -> performEvent_ $ fmap (\c -> liftIO $ smoothieTimeSeriesAppendWithCurrentTime ts c) e
  return $ (s, map snd ets)

smoothieChartChangeTimeSeriesConfig :: SmoothieChart -> TimeSeries -> SmoothieTimeSeriesConfig -> IO ()
smoothieChartChangeTimeSeriesConfig sc t cfg = do
  tsc <- smoothieGetTimeSeriesOptions_ (unSmoothieChart sc) (unTimeSeries t)
  lw <- toJSRef (_smoothieTimeSeriesConfig_lineWidth cfg)
  ss <- toJSRef (_smoothieTimeSeriesConfig_strokeStyle cfg)
  fs <- case _smoothieTimeSeriesConfig_fillStyle cfg of
             Nothing -> return jsUndefined
             Just fs' -> toJSRef fs'
  setProp ("fillStyle" :: String) fs tsc
  setProp ("strokeStyle" :: String) ss tsc
  setProp ("lineWidth" :: String) lw tsc

smoothieChartTimeSeriesSetConfig :: MonadWidget t m => SmoothieChart -> TimeSeries -> Event t SmoothieTimeSeriesConfig -> m ()
smoothieChartTimeSeriesSetConfig sc ts e = performEvent_ $ fmap (liftIO . smoothieChartChangeTimeSeriesConfig sc ts) e
