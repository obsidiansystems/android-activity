{-# OPTIONS_GHC -fno-warn-orphans #-}
module Focus.Backend.Schema where

import Data.Aeson
import Database.Groundhog.Core
import Database.Groundhog.Generic.Sql

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
