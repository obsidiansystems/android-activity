{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, EmptyDataDecls, MultiParamTypeClasses, TypeFamilies, DeriveGeneric, FlexibleInstances, FunctionalDependencies #-}
module Focus.Patch where

import Control.Monad
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics

import Focus.Schema ()

patchMap :: Ord k => Map k (Maybe v) -> Map k v -> Map k v
patchMap = Map.mergeWithKey (\_ p _ -> p) (Map.mapMaybe id) id

patchMapWith :: Ord k => (p -> Maybe v) -> (p -> v -> v) -> Map k (Maybe p) -> Map k v -> Map k v
patchMapWith i f = Map.mergeWithKey (\_ p v -> maybe (\_ -> Nothing) ((Just.) . f) p v) (Map.mapMaybe (join . fmap i)) id

 -- TODO: Use a version of makeJson that can handle instance heads instead of aeson's generic deriving
newtype SetPatch a = SetPatch (Map a Bool)
  deriving (Show, Read, Eq, Ord, Generic, Monoid, Semigroup)

instance (Ord a, ToJSON a) => ToJSON (SetPatch a)
instance (Ord a, FromJSON a) => FromJSON (SetPatch a)

applySetPatch :: Ord a => SetPatch a -> Set a -> Set a
applySetPatch (SetPatch sp) set = Map.keysSet (Map.filter id (Map.union sp (Map.fromSet (\_ -> True) set)))

mapIntersectionWithKeysSet :: Ord k => Map k v -> Set k -> Map k v
mapIntersectionWithKeysSet m s = Map.intersection m $ Map.fromSet (const ()) s

mapIntersectionWithSetMap :: (Ord k, Ord k') => Map k v -> Map k' (Set k) -> Map k v
mapIntersectionWithSetMap m sm = mapIntersectionWithKeysSet m $ Set.unions $ Map.elems sm

applyTypeaheadPatch :: (Ord k, Ord a) => Map k (Maybe (Set a, Set a)) -> Map k (Set a) -> Map k (Set a)
applyTypeaheadPatch resultsPatch results = Map.mergeWithKey (\_ p v -> mergeTypeaheadResults p v) (Map.map fst . Map.mapMaybe id) id resultsPatch results
  where
    mergeTypeaheadResults p v = case p of
      Nothing -> Nothing
      Just (adds, removes) -> Just $ Set.union adds $ Set.difference v removes

intersectTypeaheadResults :: (Ord k, Ord k')
                          => Map k v -- All values
                          -> Set k' -- Queries in view selector
                          -> Map k' (Set k) -- Map of query results
                          -> Map k v -- Filtered values
intersectTypeaheadResults vall vsq vqr = Map.intersection vall . Map.fromSet (const ()) . Set.unions . Map.elems . Map.intersection vqr $ Map.fromSet (const ()) vsq

