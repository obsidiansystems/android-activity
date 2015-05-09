-- | Utilities for running Postgres in a local directory
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Focus.Backend.DB.Local where

import System.Which

import System.Directory
import System.Exit
import System.IO
import System.Process
import Control.Exception
import Control.Monad
import Data.Monoid
import Data.Function
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Search as BS

initLocalPostgres :: FilePath -> IO ()
initLocalPostgres dbDir = do
  (_, _, _, initdb) <- runInteractiveProcess $(staticWhich "initdb")
    [ "-D", dbDir
    , "-U", "postgres"
    , "--no-locale"
    , "-E", "UTF8"
    ] Nothing Nothing
  ExitSuccess <- waitForProcess initdb
  return ()

getLocalPostgresConnectionString :: FilePath -> IO ByteString
getLocalPostgresConnectionString dbDir = do
  absoluteDbDir <- makeAbsolute dbDir
  return $ "postgresql://postgres@" <> (LBS.toStrict $ BS.replace "/" ("%2F" :: LBS.ByteString) $ encodeUtf8 $ T.pack absoluteDbDir) <> "/postgres"

startLocalPostgres :: FilePath -> IO ProcessHandle
startLocalPostgres dbDir = do
  absoluteDbDir <- makeAbsolute dbDir
  (_, _, err, postgres) <- runInteractiveProcess $(staticWhich "postgres")
    [ "-h", ""
    , "-D", absoluteDbDir
    , "-k", absoluteDbDir
    ] Nothing Nothing
  fix $ \loop -> do
    l <- hGetLine err
    let (tag, rest) = span (/= ':') l
    when (tag /= "LOG") $ fail $ "createTempDb: Unexpected output from postgres: " <> show l
    when (rest /= ":  database system is ready to accept connections") loop
  return postgres

stopLocalPostgres :: ProcessHandle -> IO ()
stopLocalPostgres postgres = do
  terminateProcess postgres
  _ <- waitForProcess postgres
  return ()

withLocalPostgres :: FilePath -> (ByteString -> IO a) -> IO a
withLocalPostgres dbDir a = bracket (startLocalPostgres dbDir) stopLocalPostgres $ \_ -> do
  dbUri <- getLocalPostgresConnectionString dbDir
  a dbUri
