module Focus.Backend
    ( withFocus
    , withDb
    ) where

import Focus.Backend.DB

import Data.Pool
import Database.Groundhog.Postgresql
import System.IO
import System.Directory (doesFileExist)
import qualified Data.ByteString.Char8 as BS
import Gargoyle
import Gargoyle.PostgreSQL.Nix

-- | withFocus is utiliized to prevent interleaving, revealing the structure
-- of errors, and decreasing total delay
withFocus :: IO a -> IO a
withFocus a = do
  hSetBuffering stderr LineBuffering -- Decrease likelihood of output from multiple threads being interleaved
  putStrLn "\a" -- Ring the bell; this is mostly helpful in development; probably should be moved to a script in focus instead of the actual server start
  a

 --TODO: Support a remote as well as local databases
 -- | withDb takes a String, which represents the path to a database, and a
 -- function that returns database connection information as arguements in
 -- order to open and start the database. Otherwise, it will create the
 -- database for you if it doesn't exist.
withDb :: String -> (Pool Postgresql -> IO a) -> IO a
withDb dbPath a = do
  dbExists <- doesFileExist dbPath
  if dbExists
    -- use the file contents as the uri for an existing server
    then BS.readFile dbPath >>= openDb . head . BS.lines >>= a
    -- otherwise assume its a folder for a local database
    else do
      g <- postgresNix
      withGargoyle g dbPath $ \dbUri -> a =<< openDb dbUri
