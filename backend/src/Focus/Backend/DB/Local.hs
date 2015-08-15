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
import Data.List

import System.FilePath
import Network.Socket
import Control.Concurrent
import System.IO.Error
import System.Posix.Signals
import System.Process.Internals
import Data.Typeable

import System.Environment
import Language.Haskell.TH.Syntax
import qualified Codec.Archive.Tar as Tar
import Data.FileEmbed
import System.IO.Temp
import Paths_focus_backend

import Debug.Trace

initLocalPostgres :: FilePath -> IO ()
initLocalPostgres dbDir = do
  (_, _, _, initdb) <- runInteractiveProcess $(staticWhich "initdb")
    [ "-D", dbDir </> "db"
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

-- | Perform a "Smart Shutdown" of Postgres; see http://www.postgresql.org/docs/current/static/server-shutdown.html
shutdownPostgresSmart :: ProcessHandle -> IO ()
shutdownPostgresSmart postgres = do
  terminateProcess postgres
  _ <- waitForProcess postgres
  return ()

-- | Perform a "Fast Shutdown" of Postgres; see http://www.postgresql.org/docs/current/static/server-shutdown.html
shutdownPostgresFast :: ProcessHandle -> IO ()
shutdownPostgresFast postgres = do
  withProcessHandle postgres $ \p -> do
    case p of
      ClosedHandle _ -> return ()
      OpenHandle h -> signalProcess sigINT h
  _ <- waitForProcess postgres
  return ()

psqlLocal :: FilePath -> IO ()
psqlLocal p = withLocalPostgres p $ \dbUri -> do
  (_, _, _, psql) <- createProcess $ proc $(staticWhich "psql")
    [ T.unpack $ decodeUtf8 dbUri
    ]
  ExitSuccess <- waitForProcess psql
  return ()

-- | Serve a local postgres database over a domain socket; the postgres server will automatically be stopped when no clients remain
serveLocalPostgres :: FilePath -> IO ()
serveLocalPostgres dbDir = do
  -- Clients must maintain a connection to controlSocket to ensure that the server doesn't get shut down
  controlSocket <- socket AF_UNIX Stream defaultProtocol
  let socketPath = dbDir </> "control"
      createSocket = do
        result <- try $ bindSocket controlSocket $ SockAddrUnix socketPath
        case result of
          Right () -> return ()
          Left e
            | isAlreadyInUseError e
            -> do
              hPutStrLn stderr $ "Error: control socket already exists; that probably means that another instance of serveLocalPostgres is already running"
              throwIO e
  bracket createSocket (\_ -> removeFile socketPath) $ \_ -> do
    -- Between bind and listen, the socket will be in a non-accepting state; this should last a very brief time, so the client should just briefly wait and then retry
    listen controlSocket 128
    numClientsVar <- newMVar (0 :: Int) --TODO: There is a failure mode here: if an interloper connects and disconnects before the initial caller connects, the initial caller will fail to connect; instead, we should start up with an existing connection (possibly a pipe passed in from the parent process) and with this var set to 1
    -- When this var is filled, the server will shut down
    shutdownVar <- newEmptyMVar
    forkIO $ forever $ do
      (s, _) <- accept controlSocket
      --TODO: What happens if we decide we're shutting down here?
      modifyMVar_ numClientsVar $ \n -> do
        return $ succ n
      forkIO $ do
        h <- socketToHandle s ReadMode
        -- Block until we hit EOF; if we successfully read a character, that means the client is in violation of the protocol, so we shut them down, too
        catchJust (\e -> if isEOFError e then Just () else Nothing) (hGetChar h >> hPutStrLn stderr "Warning: client sent data over the control socket") return
        mask_ $ do
          n <- takeMVar numClientsVar
          case pred n of
            0 -> putMVar shutdownVar ()
            n' -> putMVar numClientsVar n'
    bracket (startLocalPostgres (dbDir </> "db")) shutdownPostgresFast $ \_ -> do
      putStrLn "" -- Signal to the invoker that we're ready
      takeMVar shutdownVar

-- | Connect to a local postgres database instance; if it is not yet running, start it, then connect
withLocalPostgres :: FilePath -> (ByteString -> IO a) -> IO a
withLocalPostgres dbDir a = do
  s <- socket AF_UNIX Stream defaultProtocol
  let acquire = do
        connectResult <- try $ connect s $ SockAddrUnix $ dbDir </> "control"
        case connectResult of
          Right () -> return ()
          Left e
            | isDoesNotExistError e
            -> do (Just serverIn, Just serverOut, Nothing, server) <- withIoProc 'serveLocalPostgres dbDir $ \ioProc -> createProcess $
                    ioProc
                     { std_in = CreatePipe
                     , std_out = CreatePipe
                     , std_err = Inherit
                     }
                  forkIO $ void $ waitForProcess server
                  hClose serverIn
                  hGetLine serverOut
                  acquire -- Try again
  bracket_ acquire (shutdown s ShutdownBoth >> sClose s) $ do
    dbUri <- getLocalPostgresConnectionString $ dbDir </> "db"
    a dbUri
            

withIoProc :: (Show a, Read a) => Name -> a -> (CreateProcess -> IO b) -> IO b --TODO: Check that the thing referred to by the Name takes an argument of type a
withIoProc = $(do
  ghcPath <- runIO getExecutablePath
  args <- runIO getArgs
  let interactiveFlag = "--interactive"
  if any (== interactiveFlag) args
    then do let strippedArgs = filter (\x -> x /= interactiveFlag) args --TODO: Capture current directory?
            [| \n x io -> io $ proc ghcPath $ strippedArgs ++ ["-e", showName n ++ " (read " ++ show (show x) ++ ")"] |]
    else if any (== "-e") args
         then do let stripE [] = []
                     stripE ("-e":t) = stripE $ drop 1 t
                     stripE (h:t) = h : stripE t
                     strippedArgs = stripE args
                 [| \n x io -> io $ proc ghcPath $ strippedArgs ++ ["-e", showName n ++ " (read " ++ show (show x) ++ ")"] |]
         else do let extractPackageDBs [] = []
                     extractPackageDBs ("-package-db" : db : t) = db : extractPackageDBs t
                     extractPackageDBs (h : t) = extractPackageDBs t
                     extractPackages [] = []
                     extractPackages ("-package-id" : p : t) = p : extractPackages t
                     extractPackages (h : t) = extractPackages t
                 libDir <- runIO getLibDir
                 runIO $ print args
                 packageDBTars <- runIO $ forM (extractPackageDBs args) $ \db -> do
                   let filename = takeFileName db
                   entries <- Tar.pack (takeDirectory db) [filename]
                   return (filename, LBS.toStrict $ Tar.write entries)
                 let strippedArgs = filter ("-B" `isPrefixOf`) args ++ ["-hide-all-packages", "-no-user-package-db"] ++ concatMap (\p -> ["-package-id", p]) (extractPackages args) ++ ["-package", "focus-backend"] --TODO: Get the current package name programmatically
                 [| \n x io -> do
                      tmpDir <- getTemporaryDirectory
                      let go [] revDbStack = do
                            dbPath <- createTempDirectory tmpDir "packagedb"
                            Tar.unpack dbPath . foldr Tar.Next (Tar.Done :: Tar.Entries SomeException) =<< Tar.pack (takeDirectory libDir) ["package.conf.d"] -- Use tar to recursively copy for consistency
                            (_, _, _, ghcPkg) <- runInteractiveProcess $(staticWhich "ghc-pkg") ["recache", "--package-db", dbPath </> "package.conf.d"] Nothing Nothing
                            _ <- waitForProcess ghcPkg
                            io $ proc ghcPath $ strippedArgs ++ concatMap (\db -> ["-package-db", db]) (reverse $ (dbPath </> "package.conf.d") : revDbStack) ++ ["-e", showName n ++ " (read " ++ show (show x) ++ ") `Control.Exception.finally` mapM System.Directory.removeDirectoryRecursive " ++ show (dbPath : map takeDirectory revDbStack)]
                          go ((filename, tar):t) revDbStack = do
                            dbPath <- createTempDirectory tmpDir "packagedb"
                            Tar.unpack dbPath $ Tar.read $ LBS.fromStrict tar
                            go t $ (dbPath </> filename) : revDbStack
                      go $(liftM ListE $ mapM (\(fp, bs) -> liftM TupE $ sequence [lift fp, bsToExp bs]) packageDBTars) []
                  |]
  )
