{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Focus.Backend.DB where

import Focus.Schema
import Focus.Backend.Schema.TH

--import Database.Groundhog
--import Database.Groundhog.TH
import Database.Groundhog.Core
import Database.Groundhog.Expression
--import Database.Groundhog.Generic
--import Database.Groundhog.Instances

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Data.Time
import Control.Arrow

import Data.Pool
import Database.PostgreSQL.Simple
import Database.Groundhog.Postgresql
import Control.Monad.Trans.Control
import Control.Monad.Logger
import Control.Monad.IO.Class
import Data.ByteString (ByteString)

-- | Will return all matching instances of the given constructor
selectMap :: forall a (m :: * -> *) v (c :: (* -> *) -> *) t.
             (ProjectionDb t (PhantomDb m),
              ProjectionRestriction t (RestrictionHolder v c), DefaultKeyId v,
              Projection t v, PersistField (DefaultKey v),
              EntityConstr v c,
              HasSelectOptions a (PhantomDb m) (RestrictionHolder v c),
              PersistBackend m, PersistEntity v, Ord (IdData v),
              AutoKey v ~ DefaultKey v) =>
             t -> a -> m (Map (Id v) v)
--selectMap :: (PersistBackend m, PersistEntity v, EntityConstr v c, Constructor c, Projection (c (ConstructorMarker v)) (PhantomDb m) (RestrictionHolder v c) v, HasSelectOptions opts (PhantomDb m) (RestrictionHolder v c), AutoKey v ~ DefaultKey v, DefaultKeyId v, Ord (IdData v)) => c (ConstructorMarker v) -> opts -> m (Map (Id v) v)
selectMap constr = liftM (Map.fromList . map (first toId)) . project (AutoKeyField, constr)

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

openDb :: ByteString -> IO (Pool Postgresql)
openDb dbUri = do
  let openPostgresql = liftM Postgresql $ connectPostgreSQL dbUri
      closePostgresql (Postgresql p) = close p
  createPool openPostgresql closePostgresql 1 5 20

runDb :: ( MonadIO m
         , MonadBaseControl IO m
         , ConnectionManager cm conn
         )
      => DbPersist conn (NoLoggingT m) b
      -> Pool cm
      -> m b
runDb a dbConns = withResource dbConns $ runDbConn a

