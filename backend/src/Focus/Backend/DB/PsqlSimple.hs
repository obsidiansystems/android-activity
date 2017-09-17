{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Focus.Backend.DB.PsqlSimple ( PostgresRaw (..)
                                   , In (..), Only (..), Values (..)
                                   , Binary (..), (:.)(..), PGArray (..)
                                   , ToRow (..), FromRow (..)
                                   , ToField (..), FromField (..)
                                   , Query (..), sql, traceQuery
                                   , liftWithConn
                                   , PostgresLargeObject (..)
                                   , withStreamedLargeObject
                                   , sqlv
                                   ) where

import Control.Exception.Lifted
import Control.Monad.Reader.Class
import Control.Monad.State as State
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Builder
import Data.Char (isSpace)
import Data.Int
import Data.IORef
import Data.Semigroup
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Database.PostgreSQL.Simple hiding (query, query_, execute, execute_, executeMany, formatQuery, returning)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as Sql
import Database.PostgreSQL.Simple.LargeObjects (Oid(..), LoFd)
import qualified Database.PostgreSQL.Simple.LargeObjects as Sql
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.IO
import System.IO.Streams (OutputStream, makeOutputStream)
import qualified System.IO.Streams as Streams

import Focus.Schema

data WrappedSqlError = WrappedSqlError
       { _wrappedSqlError_rawQuery :: BS.ByteString
       , _wrappedSqlError_formattedQuery :: BS.ByteString
       , _wrappedSqlError_error :: SqlError
       }
  deriving Show

instance Exception WrappedSqlError

rethrowWithQuery :: ToRow q => Connection -> Query -> q -> SqlError -> IO a
rethrowWithQuery conn psql qs err = do
  expr <- Sql.formatQuery conn psql qs
  throw $ WrappedSqlError
    { _wrappedSqlError_rawQuery = fromQuery psql
    , _wrappedSqlError_formattedQuery = expr
    , _wrappedSqlError_error = err
    }

rethrowWithQueryMany :: ToRow q => Connection -> Query -> [q] -> SqlError -> IO a
rethrowWithQueryMany conn psql qs err = do
  expr <- Sql.formatMany conn psql qs
  throw $ WrappedSqlError
    { _wrappedSqlError_rawQuery = fromQuery psql
    , _wrappedSqlError_formattedQuery = expr
    , _wrappedSqlError_error = err
    }
rethrowWithQuery_ :: Query -> SqlError -> IO a
rethrowWithQuery_ psql err =
  throw $ WrappedSqlError
    { _wrappedSqlError_rawQuery = fromQuery psql
    , _wrappedSqlError_formattedQuery = fromQuery psql
    , _wrappedSqlError_error = err
    }

class PostgresRaw m where
  execute :: ToRow q => Query -> q -> m Int64
  execute_ :: Query -> m Int64
  executeMany :: ToRow q => Query -> [q] -> m Int64
  query :: (ToRow q, FromRow r) => Query -> q -> m [r]
  query_ :: FromRow r => Query -> m [r]
  formatQuery :: ToRow q => Query -> q -> m BS.ByteString
  returning :: (ToRow q, FromRow r) => Query -> [q] -> m [r]

traceQuery :: (PostgresRaw m, MonadIO m, ToRow q, FromRow r) => Query -> q -> m [r]
traceQuery p q = do
  s <- formatQuery p q
  liftIO (BSC.putStrLn s)
  query p q

instance MonadIO m => PostgresRaw (DbPersist Postgresql m) where
  execute psql qs = liftWithConn $ \conn -> do
    Sql.execute conn psql qs `catch` rethrowWithQuery conn psql qs
  execute_ psql = liftWithConn $ \conn -> Sql.execute_ conn psql `catch` rethrowWithQuery_ psql
  executeMany psql qs = liftWithConn $ \conn -> Sql.executeMany conn psql qs `catch` rethrowWithQueryMany conn psql qs
  query psql qs = liftWithConn $ \conn -> Sql.query conn psql qs `catch` rethrowWithQuery conn psql qs
  query_ psql = liftWithConn $ \conn -> Sql.query_ conn psql `catch` rethrowWithQuery_ psql
  formatQuery psql qs = liftWithConn $ \conn -> Sql.formatQuery conn psql qs
  returning psql qs = liftWithConn $ \conn -> Sql.returning conn psql qs `catch` rethrowWithQueryMany conn psql qs

liftWithConn :: MonadIO m
             => (Connection -> IO a)
             -> DbPersist Postgresql m a
liftWithConn f = DbPersist $ do
  (Postgresql conn) <- ask
  liftIO (f conn)

instance Semigroup Query where
  (<>) = mappend

instance (Monad m, PostgresRaw m) => PostgresRaw (StateT s m) where
  execute psql qs = lift $ execute psql qs
  execute_ = lift . execute_
  executeMany psql qs = lift $ executeMany psql qs
  query psql qs = lift $ query psql qs
  query_ = lift . query_
  formatQuery psql qs = lift $ formatQuery psql qs
  returning psql qs = lift $ returning psql qs

instance (Monad m, PostgresRaw m) => PostgresRaw (Strict.StateT s m) where
  execute psql qs = lift $ execute psql qs
  execute_ = lift . execute_
  executeMany psql qs = lift $ executeMany psql qs
  query psql qs = lift $ query psql qs
  query_ = lift . query_
  formatQuery psql qs = lift $ formatQuery psql qs
  returning psql qs = lift $ returning psql qs

instance (Monad m, PostgresRaw m) => PostgresRaw (MaybeT m) where
  execute psql qs = lift $ execute psql qs
  execute_ = lift . execute_
  executeMany psql qs = lift $ executeMany psql qs
  query psql qs = lift $ query psql qs
  query_ = lift . query_
  formatQuery psql qs = lift $ formatQuery psql qs
  returning psql qs = lift $ returning psql qs

--------------------------
-- Large object support --
--------------------------

class PostgresRaw m => PostgresLargeObject m where
  -- | Create a new postgres large object, returning its object id.
  newEmptyLargeObject :: m LargeObjectId
  -- | Act on a large object given by id, opening and closing the file descriptor appropriately.
  withLargeObject :: LargeObjectId -> IOMode -> (LoFd -> m a) -> m a
  -- | Import a file into the database as a large object.
  newLargeObjectFromFile :: FilePath -> m LargeObjectId
  -- | Given a strict ByteString, create a postgres large object and fill it with those contents.
  newLargeObjectBS :: BS.ByteString -> m LargeObjectId
  -- | Given a lazy ByteString, create a postgres large object and fill it with those contents.
  -- Also returns the total length of the data written.
  newLargeObjectLBS :: LBS.ByteString -> m (LargeObjectId, Int)
  -- | Stream the contents of a database large object to the given output stream. Useful with Snap's addToOutput.
  streamLargeObject :: LargeObjectId -> OutputStream Builder -> m ()
  -- | Stream the contents of a database large object to the given output stream. Useful with Snap's addToOutput.
  streamLargeObjectRange :: LargeObjectId -> Int -> Int -> OutputStream Builder -> m ()
  -- | Deletes the large object with the specified object id.
  deleteLargeObject :: LargeObjectId -> m ()

fromOid :: Oid -> LargeObjectId
fromOid (Oid n) = LargeObjectId (fromIntegral n)

toOid :: LargeObjectId -> Oid
toOid (LargeObjectId n) = Oid (fromIntegral n)

instance (MonadIO m, MonadBaseControl IO m) => PostgresLargeObject (DbPersist Postgresql m) where
  newEmptyLargeObject = fmap fromOid $ liftWithConn $ \conn -> Sql.loCreat conn
  withLargeObject oid mode f =
    bracket (liftWithConn $ \conn -> Sql.loOpen conn (toOid oid) mode)
            (\lofd -> liftWithConn $ \conn -> Sql.loClose conn lofd)
            f
  newLargeObjectFromFile filePath = do
    liftWithConn $ \conn -> fmap fromOid $ Sql.loImport conn filePath
  newLargeObjectBS contents = do
    oid <- newEmptyLargeObject
    n <- withLargeObject oid WriteMode $ \lofd -> liftWithConn $ \conn -> Sql.loWrite conn lofd contents
    let l = BS.length contents
    when (n /= l) . liftIO . throwIO . AssertionFailed $
      "newLargeObjectBS: loWrite reported writing " <> show n <> " bytes, expected " <> show l <> "."
    return oid
  newLargeObjectLBS contents = do
    oid <- newEmptyLargeObject
    t <- withLargeObject oid WriteMode $ \lofd -> do
      forM_ (LBS.toChunks contents) $ \chunk -> do
        n <- liftWithConn $ \conn -> Sql.loWrite conn lofd chunk
        let l = BS.length chunk
        when (n /= l) . throwIO . AssertionFailed $
          "newLargeObjectLBS: loWrite reported writing " <> show n <> " bytes, expected " <> show l <> "."
      liftWithConn $ \conn -> Sql.loTell conn lofd
    return (oid, t)
  streamLargeObject oid os =
    withLargeObject oid ReadMode $ \lofd ->
      fix $ \again -> do
        chunk <- readLargeObject lofd 8192 -- somewhat arbitrary
        case BS.length chunk of
          0 -> return ()
          _ -> do
            liftIO $ Streams.write (Just $ byteString chunk) os
            again
  streamLargeObjectRange oid start end os =
    withLargeObject oid ReadMode $ \lofd -> do
      _ <- liftWithConn $ \conn -> Sql.loSeek conn lofd Sql.AbsoluteSeek start
      let again n = do
            let nextChunkSize = min 8192 (end - n)
            chunk <- readLargeObject lofd nextChunkSize
            case BS.length chunk of
              0 -> return ()
              k -> do
                liftIO $ Streams.write (Just $ byteString chunk) os
                again (n + k)
      again start

  deleteLargeObject oid = liftWithConn $ \conn -> Sql.loUnlink conn $ toOid oid

-- Read a chunk of an opened large object. Returns Nothing when there's an error such as the end of file.
-- NB: postgresql-simple seems to have a less useful type here than postgresql-libpq...
readLargeObject :: MonadIO m => LoFd -> Int -> DbPersist Postgresql m BS.ByteString
readLargeObject lofd size = liftWithConn $ \conn -> Sql.loRead conn lofd size

instance (Monad m, PostgresLargeObject m) => PostgresLargeObject (StateT s m) where
  newEmptyLargeObject = lift newEmptyLargeObject
  withLargeObject oid mode f = do
    s <- State.get
    (v,s') <- lift $ withLargeObject oid mode (\lofd -> runStateT (f lofd) s)
    put s'
    return v
  newLargeObjectFromFile = lift . newLargeObjectFromFile
  newLargeObjectBS = lift . newLargeObjectBS
  newLargeObjectLBS = lift . newLargeObjectLBS
  streamLargeObject oid os = lift (streamLargeObject oid os)
  streamLargeObjectRange oid start end os = lift (streamLargeObjectRange oid start end os)
  deleteLargeObject = lift . deleteLargeObject

instance (Monad m, PostgresLargeObject m) => PostgresLargeObject (Strict.StateT s m) where
  newEmptyLargeObject = lift newEmptyLargeObject
  withLargeObject oid mode f = do
    s <- Strict.get
    (v,s') <- lift $ withLargeObject oid mode (\lofd -> Strict.runStateT (f lofd) s)
    put s'
    return v
  newLargeObjectFromFile = lift . newLargeObjectFromFile
  newLargeObjectBS = lift . newLargeObjectBS
  newLargeObjectLBS = lift . newLargeObjectLBS
  streamLargeObject oid os = lift (streamLargeObject oid os)
  streamLargeObjectRange oid start end os = lift (streamLargeObjectRange oid start end os)
  deleteLargeObject = lift . deleteLargeObject

instance (Monad m, PostgresLargeObject m) => PostgresLargeObject (MaybeT m) where
  newEmptyLargeObject = lift newEmptyLargeObject
  withLargeObject oid mode f =
    MaybeT $ withLargeObject oid mode (\lofd -> runMaybeT (f lofd))
  newLargeObjectFromFile = lift . newLargeObjectFromFile
  newLargeObjectBS = lift . newLargeObjectBS
  newLargeObjectLBS = lift . newLargeObjectLBS
  streamLargeObject oid os = lift (streamLargeObject oid os)
  streamLargeObjectRange oid start end os = lift (streamLargeObjectRange oid start end os)
  deleteLargeObject = lift . deleteLargeObject

withStreamedLargeObject :: (PostgresLargeObject m, MonadIO m) => LargeObjectId -> (LBS.ByteString -> IO ()) -> m ()
withStreamedLargeObject oid f = do
  lo <- liftIO $ newIORef mempty
  cb <- liftIO $ makeOutputStream $ \case
    Just chunk -> modifyIORef lo $ \chunks -> chunks <> chunk
    Nothing -> do
      payload <- readIORef lo
      f $ BS.toLazyByteString payload
  streamLargeObject oid cb
  liftIO $ Streams.write Nothing cb

---------------------------------
-- PostgreSQL.Simple instances --
---------------------------------

instance (FromField (IdData a)) => FromField (Id a) where
  fromField f mbs = fmap Id (fromField f mbs)

instance (ToField (IdData a)) => ToField (Id a) where
  toField (Id x) = toField x

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k) =>
    FromRow (a,b,c,d,e,f,g,h,i,j,k) where
    fromRow = (,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                           <*> field <*> field <*> field <*> field <*> field
                           <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l) =>
    FromRow (a,b,c,d,e,f,g,h,i,j,k,l) where
    fromRow = (,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                            <*> field <*> field <*> field <*> field <*> field
                            <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l, FromField m) =>
    FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    fromRow = (,,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                             <*> field <*> field <*> field <*> field <*> field
                             <*> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l, FromField m, FromField n) =>
    FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    fromRow = (,,,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                              <*> field <*> field <*> field <*> field <*> field
                              <*> field <*> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l, FromField m, FromField n, FromField o) =>
    FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    fromRow = (,,,,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                               <*> field <*> field <*> field <*> field <*> field
                               <*> field <*> field <*> field <*> field <*> field

-- | This quasiquoter takes a SQL query with named arguments in the form "?var" and generates a pair
-- consisting of the Query string itself and a tuple of variables in corresponding order.
--
-- For example: uncurry query [sqlv| SELECT * FROM 'Book' b WHERE b.title = ?title AND b.year = ?year |]
--
-- will be equivalent to query [sql| SELECT * FROM 'Book' b WHERE b.title = ? AND b.year = ? |] (title,year)
sqlv :: QuasiQuoter
sqlv = QuasiQuoter
    { quotePat  = error "Focus.Backend.DB.sqlv:\
                        \ quasiquoter used in pattern context"
    , quoteType = error "Focus.Backend.DB.sqlv:\
                        \ quasiquoter used in type context"
    , quoteExp  = sqlvExp
    , quoteDec  = error "Focus.Backend.DB.sqlv:\
                        \ quasiquoter used in declaration context"
    }

sqlvExp :: String -> Q Exp
sqlvExp s =
  let (s',vs) = extractVars s
  in tupE [appE [| fromString :: String -> Query |] . stringE . minimizeSpace $ s', tupE (map varE vs)]

extractVars :: String -> (String, [Name])
extractVars s = extractVars' s
  where 
    extractVars' [] = ([],[])
    extractVars' ('?':s') =
      let (var,rest) = break isSpace s'
          (s'',vars) = extractVars' rest
      in ('?':s'', mkName var : vars)
    extractVars' s' =
      let (pre,post) = break (=='?') s'
          (s'',vars) = extractVars' post
      in (pre ++ s'', vars)

minimizeSpace :: String -> String
minimizeSpace = drop 1 . reduceSpace
  where
    needsReduced []          = False
    needsReduced ('-':'-':_) = True
    needsReduced (x:_)       = isSpace x

    reduceSpace xs =
        case dropWhile isSpace xs of
          [] -> []
          ('-':'-':ys) -> reduceSpace (dropWhile (/= '\n') ys)
          ys -> ' ' : insql ys

    insql ('\'':xs)            = '\'' : instring xs
    insql xs | needsReduced xs = reduceSpace xs
    insql (x:xs)               = x : insql xs
    insql []                   = []

    instring ('\'':'\'':xs) = '\'':'\'': instring xs
    instring ('\'':xs)      = '\'': insql xs
    instring (x:xs)         = x : instring xs
    instring []             = error "Focus.Backend.DB.PsqlSimple.sqlv:\
                                    \ string literal not terminated"
