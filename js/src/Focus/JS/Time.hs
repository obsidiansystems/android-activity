{-# LANGUAGE FlexibleContexts #-}
module Focus.JS.Time where

import Focus.JS.Request
import Foreign.JavaScript.TH
import Reflex.Dom

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary.Get
import Data.Time
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import qualified Data.ByteString.Lazy as LBS

getTimeZoneSeries :: (Monad m, MonadIO m, HasJS x m, HasJS x (WidgetHost m)) => String -> m (Maybe TimeZoneSeries)
getTimeZoneSeries path = do
  dVar <- liftIO newEmptyMVar
  liftJS $ mkBinaryGet path $ putMVar dVar
  d <- liftIO $ takeMVar dVar
  liftIO $ return $ olsonToTimeZoneSeries $ runGet (getOlson noLimits) $ LBS.fromStrict d

createDynamicTime :: MonadWidget t m => m (Dynamic t UTCTime)
createDynamicTime = do
  t <- liftIO getCurrentTime
  pb <- getPostBuild
  tn <- performEventAsync $ fmap (\_ cb -> liftIO $ void $ forkIO $ forever $ threadDelay 1000000 >> getCurrentTime >>= cb) pb
  holdDyn t tn


