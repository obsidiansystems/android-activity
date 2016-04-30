{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DefaultSignatures, EmptyDataDecls, MultiParamTypeClasses, TypeFamilies, DeriveGeneric, FlexibleInstances, FlexibleContexts, FunctionalDependencies, StandaloneDeriving #-}
module Focus.Patch where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable
import GHC.Generics

import Focus.AppendMap (AppendMap (..))
import qualified Focus.AppendMap as AppendMap
import Focus.Schema ()

class Patchable v where
  type Patch v :: *
  type instance Patch v = v
  -- patch should be a left {semigroup, monoid} action if the Patch type is one
  patch :: Patch v -> v -> v
  default patch :: v -> v -> v
  patch v _ = v
  patchToValue :: Patch v -> Maybe v
  default patchToValue :: v -> Maybe v
  patchToValue = Just

instance Ord v => Patchable (Set v) where
  type Patch (Set v) = SetPatch v
  patch (SetPatch sp) s = Map.keysSet (Map.filter id (Map.union sp (Map.fromSet (\_ -> True) s)))
  patchToValue p = Just (patch p Set.empty)

instance (Ord k, Patchable v) => Patchable (Map k v) where
  type Patch (Map k v) = MapPatch k v
  patch (MapPatch mp) mv =
    Map.mergeWithKey
      (\_ p v -> maybe (\_ -> Nothing) ((Just.) . patch) p v)
      (Map.mapMaybe (join . fmap patchToValue))
      id mp mv
  patchToValue p = Just (patch p Map.empty)

instance (Ord k, Patchable v, Semigroup (Patch v)) => Patchable (AppendMap k v) where
  type Patch (AppendMap k v) = AppendMapPatch k v
  patch (AppendMapPatch (AppendMap mp)) (AppendMap mv) = AppendMap $
    Map.mergeWithKey
      (\_ p v -> maybe (\_ -> Nothing) ((Just.) . patch) p v)
      (Map.mapMaybe (join . fmap patchToValue))
      id mp mv
  patchToValue p = Just (patch p AppendMap.empty)

 -- TODO: Use a version of makeJson that can handle instance heads instead of aeson's generic deriving
newtype SetPatch a = SetPatch
          { unSetPatch :: Map a Bool }
  deriving (Show, Read, Eq, Ord, Generic, Monoid, Semigroup, Typeable, ToJSON, FromJSON)

newtype MapPatch k a = MapPatch
          { unMapPatch :: Map k (Maybe (Patch a))
          }
  deriving (Generic, Typeable)

deriving instance (Ord k, Eq (Patch a)) => Eq (MapPatch k a)
deriving instance (Ord k, Ord (Patch a)) => Ord (MapPatch k a)
deriving instance (Ord k, Show k, Show (Patch a)) => Show (MapPatch k a)
deriving instance (Ord k, Read k, Read (Patch a)) => Read (MapPatch k a)
deriving instance (Ord k) => Semigroup (MapPatch k a)
deriving instance (Ord k) => Monoid (MapPatch k a)
deriving instance (ToJSON k, ToJSON (Patch a)) => ToJSON (MapPatch k a)
deriving instance (Ord k, FromJSON k, FromJSON (Patch a)) => FromJSON (MapPatch k a)

newtype AppendMapPatch k a = AppendMapPatch
          { unAppendMapPatch :: AppendMap k (Maybe (Patch a))
          }
  deriving (Generic, Typeable)

deriving instance (Ord k, Eq (Patch a)) => Eq (AppendMapPatch k a)
deriving instance (Ord k, Ord (Patch a)) => Ord (AppendMapPatch k a)
deriving instance (Ord k, Show k, Show (Patch a)) => Show (AppendMapPatch k a)
deriving instance (Ord k, Read k, Read (Patch a)) => Read (AppendMapPatch k a)
deriving instance (Ord k, Semigroup (Patch a)) => Semigroup (AppendMapPatch k a)
deriving instance (Ord k, Semigroup (Patch a)) => Monoid (AppendMapPatch k a)
deriving instance (ToJSON k, ToJSON (Patch a)) => ToJSON (AppendMapPatch k a)
deriving instance (Ord k, FromJSON k, FromJSON (Patch a)) => FromJSON (AppendMapPatch k a)

mapIntersectionWithKeysSet :: Ord k => Map k v -> Set k -> Map k v
mapIntersectionWithKeysSet m s = Map.intersection m $ Map.fromSet (const ()) s

mapIntersectionWithSetMap :: (Ord k, Ord k') => Map k v -> Map k' (Set k) -> Map k v
mapIntersectionWithSetMap m sm = mapIntersectionWithKeysSet m $ Set.unions $ Map.elems sm

intersectTypeaheadResults :: (Ord k, Ord k')
                          => Map k v -- All values
                          -> Set k' -- Queries in view selector
                          -> Map k' (Set k) -- Map of query results
                          -> Map k v -- Filtered values
intersectTypeaheadResults vall vsq vqr = Map.intersection vall . Map.fromSet (const ()) . Set.unions . Map.elems . Map.intersection vqr $ Map.fromSet (const ()) vsq

makeWrapped ''SetPatch
makeWrapped ''MapPatch

instance Patchable Text
