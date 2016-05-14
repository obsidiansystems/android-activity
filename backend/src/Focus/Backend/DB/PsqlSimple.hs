{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Focus.Backend.DB.PsqlSimple ( PostgresRaw (..)
                                   , In (..), Only (..), Values (..)
                                   , Binary (..), (:.)(..), PGArray (..)
                                   , ToRow (..), FromRow (..)
                                   , ToField (..), FromField (..)
                                   , Query (..), sql
                                   ) where

import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Int
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

class PostgresRaw m where
  execute :: ToRow q => Query -> q -> m Int64
  execute_ :: Query -> m Int64
  query :: (ToRow q, FromRow r) => Query -> q -> m [r]
  query_ :: FromRow r => Query -> m [r]
  formatQuery :: ToRow q => Query -> q -> m ByteString

instance MonadIO m => PostgresRaw (DbPersist Postgresql m) where
  execute psql qs = liftWithConn $ \conn -> Sql.execute conn psql qs
  execute_ psql = liftWithConn $ \conn -> Sql.execute_ conn psql
  query psql qs = liftWithConn $ \conn -> Sql.query conn psql qs
  query_ psql = liftWithConn $ \conn -> Sql.query_ conn psql
  formatQuery psql qs = liftWithConn $ \conn -> Sql.formatQuery conn psql qs

liftWithConn :: MonadIO m
             => (Connection -> IO a)
             -> DbPersist Postgresql m a
liftWithConn f = DbPersist $ do
  (Postgresql conn) <- ask
  liftIO (f conn)
