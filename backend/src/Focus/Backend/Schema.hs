{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Focus.Backend.Schema where

import Data.Aeson
import Database.Groundhog.Core
import Database.Groundhog.Generic.Sql ()
import Data.Functor.Identity
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types

import Focus.Backend.DB.Groundhog
import Focus.Schema (Json (..), SchemaName(..), LargeObjectId(..))

instance ToField SchemaName where
  toField (SchemaName t) = toField (Identifier t)

makePersistFieldNewtype ''SchemaName

deriving instance PrimitivePersistField SchemaName

instance NeverNull SchemaName

instance PersistField LargeObjectId where
  persistName _ = "LargeObjectId"
  toPersistValues (LargeObjectId n) = toPersistValues n
  fromPersistValues pv = do
    (x, pv') <- fromPersistValues pv
    return (LargeObjectId x, pv')
  dbType _ _ = DbTypePrimitive
    (DbOther $ OtherTypeDef [Left "oid"]) -- From https://www.postgresql.org/docs/current/static/lo-funcs.html
    False -- Not nullable
    Nothing -- No default value
    Nothing -- No parent table reference

deriving instance PrimitivePersistField LargeObjectId

instance NeverNull LargeObjectId

instance (ToJSON a, FromJSON a) => PersistField (Json a) where
  --TODO: Should this include the name of the underlying type
  persistName _ = "Json"
  toPersistValues (Json a) = toPersistValues (encode a)
  fromPersistValues vs = do
    (r, vs') <- fromPersistValues vs
    Just r' <- return $ decode' r
    return (Json r', vs')
  dbType p (Json a) = dbType p (encode a)

instance (ToJSON a, FromJSON a) => PrimitivePersistField (Json a) where
  toPrimitivePersistValue p (Json a) = toPrimitivePersistValue p (encode a)
  fromPrimitivePersistValue p v = runIdentity $ do
    Just r <- return $ decode' $ fromPrimitivePersistValue p v
    return (Json r)
