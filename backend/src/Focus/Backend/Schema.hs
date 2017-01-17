{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Focus.Backend.Schema where

import Data.Aeson
import Database.Groundhog.Core
import Database.Groundhog.Generic.Sql ()
import Data.Functor.Identity
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types

import Focus.Backend.DB.Groundhog
import Focus.Schema (Json (..), SchemaName(..))

instance ToField SchemaName where
  toField (SchemaName t) = toField (Identifier t)

makePersistFieldNewtype ''SchemaName

deriving instance PrimitivePersistField SchemaName

instance NeverNull SchemaName

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
