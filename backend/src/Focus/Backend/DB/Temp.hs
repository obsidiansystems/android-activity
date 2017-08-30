{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Focus.Backend.DB.Temp where

import System.IO.Temp
import Data.ByteString (ByteString)
import Gargoyle
import Gargoyle.PostgreSQL.Nix

withTempPostgres :: (ByteString -> IO a) -> IO a
withTempPostgres a = withSystemTempDirectory "focusTempPostgres" $ \dbDir -> do
  g <- postgresNix
  withGargoyle g dbDir a
