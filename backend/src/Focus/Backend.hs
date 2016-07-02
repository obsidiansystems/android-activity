module Focus.Backend where

import Focus.Backend.DB
import Focus.Backend.DB.Local

import Data.Pool
import Database.Groundhog.Postgresql
import System.IO

withFocus :: IO a -> IO a
withFocus a = do
  hSetBuffering stderr LineBuffering -- Decrease likelihood of output from multiple threads being interleaved
  putStrLn "\a" -- Ring the bell; this is mostly helpful in development; probably should be moved to a script in focus instead of the actual server start
  a

 --TODO: Support a remote as well as local databases
withDb :: String -> (Pool Postgresql -> IO a) -> IO a
withDb dbDir a = withLocalPostgres dbDir $ \dbUri -> do
  a =<< openDb dbUri
