{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Focus.Backend.DB.Temp where

import Focus.Backend.DB.Local

import System.IO.Temp
import Data.ByteString (ByteString)

withTempPostgres :: (ByteString -> IO a) -> IO a
withTempPostgres a = withSystemTempDirectory "focusTempPostgres" $ \dbDir -> do
  initLocalPostgres dbDir
  withLocalPostgres dbDir a
