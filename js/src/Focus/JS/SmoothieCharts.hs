{-# LANGUAGE CPP, ForeignFunctionInterface, JavaScriptFFI #-}
module Focus.JS.SmoothieCharts where
--http://smoothiecharts.org/

import Reflex
import Reflex.Dom

import Control.Monad
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHCJS.DOM.Types hiding (Event)
import GHCJS.Foreign
import GHCJS.Types

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif


newtype SmoothieChart = SmoothieChart { unSmoothieChart :: JSRef SmoothieChart }
newtype TimeSeries = TimeSeries { unTimeSeries :: JSRef TimeSeries }

-- JS(smoothieChartNew_, "new SmoothieChart({maxValueScale: 1.2})", IO (JSRef SmoothieChart))
JS(smoothieChartNew_, "new SmoothieChart()", IO (JSRef SmoothieChart))
JS(smoothieStreamTo_, "$1.streamTo($2, $3)", JSRef SmoothieChart -> JSRef Element -> Double -> IO ())
JS(smoothieTimeSeriesNew_, "new TimeSeries({resetBounds: false})", IO (JSRef TimeSeries))
JS(smoothieAddTimeSeries_, "$1.addTimeSeries($2, {lineWidth:2,strokeStyle:'#00ff00'})", JSRef SmoothieChart -> JSRef TimeSeries -> IO ())
JS(smoothieTimeSeriesAppend_, "console.log($3); $1.append($2, $3)", JSRef TimeSeries -> Double -> Double -> IO ())
JS(smoothieTimestamp_, "new Date().getTime()", IO Double)

smoothieChartNew :: IO SmoothieChart
smoothieChartNew = liftM SmoothieChart smoothieChartNew_

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


smoothieChart :: MonadWidget t m => Map String String -> Double -> [Event t Double] -> m ()
smoothieChart attrs delay es = do
  (canvas, _) <- elAttr' "canvas" attrs $ return ()
  ets <- liftIO $ do
    s <- smoothieChartNew
    ets <- forM es $ \e -> do
      ts <- smoothieTimeSeriesNew
      smoothieAddTimeSeries s ts
      return (e, ts)
    smoothieStreamTo s canvas delay
    return ets
  forM ets $ \(e, ts) -> performEvent_ $ fmap (\c -> liftIO $ smoothieTimeSeriesAppendWithCurrentTime ts c) e
  return ()

