{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Focus.Backend.DB.PsqlSimple ( PostgresRaw (..)
                                   , In (..), Only (..), Values (..)
                                   , Binary (..), (:.)(..), PGArray (..)
                                   , ToRow (..), FromRow (..)
                                   , ToField (..), FromField (..)
                                   , Query (..), sql, traceQuery
                                   , liftWithConn
                                   ) where

import Control.Exception
import Control.Monad.Reader.Class
import Control.Monad.State
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Int
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

import Focus.Schema

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
  formatQuery :: ToRow q => Query -> q -> m ByteString
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
