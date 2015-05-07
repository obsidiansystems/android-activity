{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Focus.DB.Temp where

import System.IO
import System.Exit
import System.Process
import System.Directory
import System.IO.Temp
import Control.Monad
import Control.Exception
import Data.Monoid
import Data.Function
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Search as BS

createTempDb :: IO FilePath
createTempDb = do
  tmp <- getTemporaryDirectory
  dbDir <- createTempDirectory tmp "spendhawk"
  (_, _, _, initdb) <- runInteractiveProcess "initdb"
    [ "-D", dbDir
    , "-U", "postgres"
    , "--no-locale"
    , "-E", "UTF8"
    , "-N"
    ] Nothing Nothing
  ExitSuccess <- waitForProcess initdb
  return dbDir

destroyTempDb :: FilePath -> IO ()
destroyTempDb = removeDirectoryRecursive

startTempDb :: FilePath -> IO ProcessHandle
startTempDb dbDir = do
  (_, _, err, postgres) <- runInteractiveProcess "postgres"
    [ "-h", ""
    , "-D", dbDir
    , "-k", dbDir
    ] Nothing Nothing
  fix $ \loop -> do
    l <- hGetLine err
    let (tag, rest) = span (/= ':') l
    when (tag /= "LOG") $ fail $ "createTempDb: Unexpected output from postgres: " <> show l
    when (rest /= ":  database system is ready to accept connections") loop
  return postgres

stopTempDb :: ProcessHandle -> IO ()
stopTempDb postgres = do
  terminateProcess postgres
  _ <- waitForProcess postgres
  return ()

tempDbConnectionString :: FilePath -> LBS.ByteString
tempDbConnectionString dbDir = "postgresql://postgres@" <> (BS.replace "/" ("%2F" :: LBS.ByteString) $ encodeUtf8 $ T.pack dbDir) <> "/postgres"

withTempDb :: (LBS.ByteString -> IO a) -> IO a
withTempDb a = bracket createTempDb destroyTempDb $ \dbDir -> bracket (startTempDb dbDir) stopTempDb $ \_ -> a $ tempDbConnectionString dbDir
