{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Focus.Backend.DB.PsqlSimple ( PostgresRaw (..)
                                   , In (..), Only (..), Values (..)
                                   , Binary (..), (:.)(..), PGArray (..)
                                   , ToRow (..), FromRow (..)
                                   , ToField (..), FromField (..)
                                   , Query (..), sql
                                   ) where

import Control.Exception
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Int
import Data.Semigroup
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Database.PostgreSQL.Simple hiding (query, query_, execute, execute_, executeMany, formatQuery)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as Sql

data WrappedSqlError = WrappedSqlError
       { _wrappedSqlError_rawQuery :: ByteString
       , _wrappedSqlError_formattedQuery :: ByteString
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
  query :: (ToRow q, FromRow r) => Query -> q -> m [r]
  query_ :: FromRow r => Query -> m [r]
  formatQuery :: ToRow q => Query -> q -> m ByteString

instance MonadIO m => PostgresRaw (DbPersist Postgresql m) where
  execute psql qs = liftWithConn $ \conn -> do
    Sql.execute conn psql qs `catch` rethrowWithQuery conn psql qs
  execute_ psql = liftWithConn $ \conn -> Sql.execute_ conn psql `catch` rethrowWithQuery_ psql
  query psql qs = liftWithConn $ \conn -> Sql.query conn psql qs `catch` rethrowWithQuery conn psql qs
  query_ psql = liftWithConn $ \conn -> Sql.query_ conn psql `catch` rethrowWithQuery_ psql
  formatQuery psql qs = liftWithConn $ \conn -> Sql.formatQuery conn psql qs

liftWithConn :: MonadIO m
             => (Connection -> IO a)
             -> DbPersist Postgresql m a
liftWithConn f = DbPersist $ do
  (Postgresql conn) <- ask
  liftIO (f conn)

instance Semigroup Query where
  (<>) = mappend
