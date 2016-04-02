{-# LANGUAGE DeriveGeneric #-}
module Data.Min where

import Data.Aeson
import GHC.Generics

data Min a = Min a
           | Infinity
           deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON a => ToJSON (Min a)
instance FromJSON a => FromJSON (Min a)

instance Ord a => Monoid (Min a) where
  mempty = Infinity
  mappend x y = min x y

