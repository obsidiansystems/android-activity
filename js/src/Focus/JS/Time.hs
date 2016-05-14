{-# LANGUAGE FlexibleContexts, TypeFamilies, OverloadedStrings #-}
module Focus.JS.Time where

import Focus.JS.Request
import Foreign.JavaScript.TH
import Reflex.Dom

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary.Get
import Data.Time
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Monoid

-- TODO: This just dies if the request fails, rather than resulting in Nothing.
getTimeZoneSeries :: (MonadIO m, HasJS x m) => Text -> m (Maybe TimeZoneSeries)
getTimeZoneSeries path = do
  dVar <- liftIO newEmptyMVar
  liftJS . mkBinaryGet ("zoneinfo/" <> path) $ putMVar dVar
  d <- liftIO $ takeMVar dVar
  liftIO . return . olsonToTimeZoneSeries . runGet (getOlson noLimits) $ LBS.fromStrict d

createDynamicTime :: MonadWidget t m => m (Dynamic t UTCTime)
createDynamicTime = do
  t <- liftIO getCurrentTime
  pb <- getPostBuild
  tn <- performEventAsync $ fmap (\_ cb -> liftIO $ void $ forkIO $ forever $ threadDelay 1000000 >> getCurrentTime >>= cb) pb
  holdDyn t tn
