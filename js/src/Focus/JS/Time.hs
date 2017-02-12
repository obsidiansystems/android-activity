{-# LANGUAGE FlexibleContexts, TypeFamilies, OverloadedStrings #-}
module Focus.JS.Time where

import Focus.JS.Request (mkBinaryGet)
import Focus.Time
import Reflex.Dom.Core

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix (MonadFix)
import Data.Binary.Get
import Data.Time
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid
import GHCJS.DOM.Types (MonadJSM)

-- TODO: This just dies if the request fails, rather than resulting in Nothing.
getTimeZoneSeries :: (MonadJSM m, MonadFix m) => TimeZoneName -> m (Maybe TimeZoneSeries)
getTimeZoneSeries tzName = do
  dVar <- liftIO newEmptyMVar
  _ <- mkBinaryGet ("zoneinfo/" <> unTimeZoneName tzName) $ liftIO . putMVar dVar
  d <- liftIO $ takeMVar dVar
  liftIO . return . olsonToTimeZoneSeries . runGet (getOlson noLimits) $ LBS.fromStrict d

createDynamicTime :: MonadWidget t m => m (Dynamic t UTCTime)
createDynamicTime = do
  t <- liftIO getCurrentTime
  pb <- getPostBuild
  tn <- performEventAsync $ fmap (\_ cb -> liftIO $ void $ forkIO $ forever $ threadDelay 1000000 >> getCurrentTime >>= cb) pb
  holdDyn t tn

