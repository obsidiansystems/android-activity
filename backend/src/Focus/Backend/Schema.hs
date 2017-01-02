{-# OPTIONS_GHC -fno-warn-orphans #-}
module Focus.Backend.Schema where

import Data.Aeson
import Database.Groundhog.Core
import Database.Groundhog.Generic.Sql
import Data.Functor.Identity

import Focus.Schema (Json (..))

instance (ToJSON a, FromJSON a) => PersistField (Json a) where
  --TODO: Should this include the name of the underlying type
  persistName _ = "Json"
  toPersistValues (Json a) = toPersistValues (encode a)
  fromPersistValues vs = do
    (r, vs) <- fromPersistValues vs
    Just r' <- return $ decode' r
    return (Json r', vs)
  dbType p (Json a) = dbType p (encode a)

instance (ToJSON a, FromJSON a) => PrimitivePersistField (Json a) where
  toPrimitivePersistValue p (Json a) = toPrimitivePersistValue p (encode a)
  fromPrimitivePersistValue p v = runIdentity $ do
    Just r <- return $ decode' $ fromPrimitivePersistValue p v
    return (Json r)
