{-# LANGUAGE DeriveGeneric #-}
module Data.SortedPair (SortedPair, sortedPair, map, fst, snd, other, toSet, member) where

import Prelude hiding (map, fst, snd)
import Data.Aeson
import GHC.Generics
import qualified Data.Set as Set
import Data.Set (Set)

data SortedPair a = SortedPair_LT a a
                  | SortedPair_EQ a
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON a => ToJSON (SortedPair a)
instance FromJSON a => FromJSON (SortedPair a)

map :: Ord b => (a -> b) -> SortedPair a -> SortedPair b
map f sp = case sp of
  SortedPair_LT a b -> sortedPair (f a) (f b)
  SortedPair_EQ a -> SortedPair_EQ (f a)

sortedPair :: Ord a => a -> a -> SortedPair a
sortedPair a b = case a `compare` b of
  EQ -> SortedPair_EQ a
  LT -> SortedPair_LT a b
  GT -> SortedPair_LT b a

fst :: SortedPair a -> a
fst sp = case sp of
  SortedPair_LT a _ -> a
  SortedPair_EQ a -> a

snd :: SortedPair a -> a
snd sp = case sp of
  SortedPair_LT _ b -> b
  SortedPair_EQ b -> b

other :: Eq a => a -> SortedPair a -> Maybe a
other x (SortedPair_EQ u) | x == u    = Just u
                          | otherwise = Nothing
other x (SortedPair_LT u v) | x == u = Just v
                            | x == v = Just u
                            | otherwise = Nothing

member :: Eq a => a -> SortedPair a -> Bool
member x (SortedPair_EQ a) = x == a
member x (SortedPair_LT a b) = x == a || x == b

toSet :: Ord a => SortedPair a -> Set a
toSet sp = case sp of
  SortedPair_LT a b -> Set.fromList [a,b]
  SortedPair_EQ a -> Set.singleton a