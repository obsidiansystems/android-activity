{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Focus.Backend.DB where

import Focus.AppendMap (AppendMap (..))
import Focus.Schema
import Focus.Backend.Schema ()
import Focus.Backend.Schema.TH
import Focus.Backend.DB.PsqlSimple

import Database.Groundhog.Generic hiding (runDb, bracket)
import Database.Groundhog.Core
import Database.Groundhog.Expression
import Database.Groundhog.Generic.Sql
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.Functor.Identity
import Data.Pool
import Database.Groundhog.Postgresql
import Database.PostgreSQL.Simple hiding (execute, execute_)

import Control.Arrow
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time

type FocusPersist = DbPersist Postgresql (NoLoggingT IO)

-- | Will return all matching instances of the given constructor
selectMap :: forall a (m :: * -> *) v (c :: (* -> *) -> *) t.
             (ProjectionDb t (PhantomDb m),
              ProjectionRestriction t (RestrictionHolder v c), DefaultKeyId v,
              Projection t v,
              EntityConstr v c,
              HasSelectOptions a (PhantomDb m) (RestrictionHolder v c),
              PersistBackend m, Ord (IdData v),
              AutoKey v ~ DefaultKey v) =>
             t -> a -> m (Map (Id v) v)
--selectMap :: (PersistBackend m, PersistEntity v, EntityConstr v c, Constructor c, Projection (c (ConstructorMarker v)) (PhantomDb m) (RestrictionHolder v c) v, HasSelectOptions opts (PhantomDb m) (RestrictionHolder v c), AutoKey v ~ DefaultKey v, DefaultKeyId v, Ord (IdData v)) => c (ConstructorMarker v) -> opts -> m (Map (Id v) v)
selectMap constr = liftM (Map.fromList . map (first toId)) . project (AutoKeyField, constr)

selectMap' :: forall a (m :: * -> *) v (c :: (* -> *) -> *) t.
              (ProjectionDb t (PhantomDb m),
              ProjectionRestriction t (RestrictionHolder v c), DefaultKeyId v,
              Projection t v,
              EntityConstr v c,
              HasSelectOptions a (PhantomDb m) (RestrictionHolder v c),
              PersistBackend m, Ord (IdData v),
              AutoKey v ~ DefaultKey v) =>
              t -> a -> m (AppendMap (Id v) v)
selectMap' constr = fmap AppendMap . selectMap constr

fieldIsJust, fieldIsNothing :: (NeverNull a, Expression db r f, PrimitivePersistField a, Projection f (Maybe a), Unifiable f (Maybe a)) => f -> Cond db r

--fieldIsNothing :: forall db r a b x. (r ~ RestrictionHolder a x, EntityConstr a x, Expression db r (Maybe b), Unifiable (Field a x (Maybe b)) (Maybe b), NeverNull b, PrimitivePersistField b) => Field a x (Maybe b) -> Cond db r
fieldIsNothing f = isFieldNothing f

--fieldIsJust :: forall db r a b x. (r ~ RestrictionHolder a x, EntityConstr a x, Expression db r (Maybe b), Unifiable (Field a x (Maybe b)) (Maybe b), NeverNull b, PrimitivePersistField b) => Field a x (Maybe b) -> Cond db r
--fieldIsJust f = f /=. (Nothing :: Maybe b)
fieldIsJust f = Not $ isFieldNothing f

getTime :: PersistBackend m => m UTCTime
getTime = do
  Just [PersistUTCTime t] <- queryRaw False "select current_timestamp(3) at time zone 'utc'" [] id
  return t

withTime :: PersistBackend m => (UTCTime -> m a) -> m a
withTime a = do
  now <- getTime
  a now

openDb :: ByteString -> IO (Pool Postgresql)
openDb dbUri = do
  let openPostgresql = liftM Postgresql $ connectPostgreSQL dbUri
      closePostgresql (Postgresql p) = close p
  createPool openPostgresql closePostgresql 1 5 20

class RunDb f where
  runDb :: (MonadIO m, MonadBaseControl IO m, ConnectionManager cm conn, PostgresRaw (DbPersist conn (NoLoggingT m)), PersistBackend (DbPersist conn (NoLoggingT m)))
        => f (Pool cm)
        -> DbPersist conn (NoLoggingT m) b
        -> m b

instance RunDb Identity where
  runDb (Identity db) = withResource db . runDbConn . withSchema (SchemaName "public")

instance RunDb WithSchema where
  runDb (WithSchema schema db) = withResource db . runDbConn . withSchema schema

getSearchPath :: PersistBackend m => m String
getSearchPath = do
  [searchPath] :: [String] <- queryRaw False "SHOW search_path" [] $ mapAllRows (fmap fst . fromPersistValues)
  return searchPath

setSearchPath :: (Monad m, PostgresRaw m) => String -> m ()
setSearchPath sp = void $ execute_ $ "SET search_path TO " <> fromString sp

setSchema :: (Monad m, PostgresRaw m) => SchemaName -> m ()
setSchema schema = void $ execute [sql| SET search_path TO ?,"$user",public |] (Only schema)

-- | Sets the search path to a particular schema, runs an action in that schema, and resets the search path
withSchema :: (PostgresRaw m, PersistBackend m) => SchemaName -> m r -> m r
withSchema schema a = do
  sp <- getSearchPath
  setSchema schema
  r <- a
  setSearchPath sp
  return r

ensureSchemaExists :: (Monad m, PostgresRaw m)
                   => SchemaName
                   -> m ()
ensureSchemaExists schema = void $ execute [sql| CREATE SCHEMA IF NOT EXISTS ? |] (Only schema)

ilike :: (SqlDb db, ExpressionOf db r a a') => a -> String -> Cond db r
ilike a b = CondRaw $ operator 40 " ILIKE " a b

