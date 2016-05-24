{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DefaultSignatures, EmptyDataDecls, MultiParamTypeClasses, TypeFamilies, DeriveGeneric, FlexibleInstances, FlexibleContexts, FunctionalDependencies, StandaloneDeriving, UndecidableInstances, LambdaCase #-}
module Focus.Patch where

import Control.Lens
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable
import GHC.Generics

import Focus.Schema ()

class Semigroup (Patch v) => Patchable v where
  type Patch v :: *
  type instance Patch v = First v
  -- patch should be a left {semigroup, monoid} action if the Patch type is one
  patch :: Patch v -> v -> v
  default patch :: First v -> v -> v
  patch (First v) _ = v

instance Ord v => Patchable (Set v) where
  type Patch (Set v) = SetPatch v
  patch (SetPatch sp) s = Map.keysSet (Map.filter id (Map.union sp (Map.fromSet (\_ -> True) s)))

instance (Ord k, Patchable v) => Patchable (Map k v) where
  type Patch (Map k v) = MapPatch k v
  patch (MapPatch mp) mv =
    Map.mergeWithKey
      patchOnValue
      onlyPatch
      id mp mv
   where
    patchOnValue _ p v = case p of
      ElemPatch_Remove -> Nothing
      ElemPatch_Insert v' -> Just v'
      ElemPatch_Upsert p' _ -> Just $ patch p' v
    onlyPatch = Map.mapMaybe $ \case
      ElemPatch_Remove -> Nothing
      ElemPatch_Upsert _ x -> x
      ElemPatch_Insert x -> Just x

 -- TODO: Use a version of makeJson that can handle instance heads instead of aeson's generic deriving
 -- NB: Monoid instance of Map that takes the left element is the correct monoid instance for SetPatch
newtype SetPatch a = SetPatch
          { unSetPatch :: Map a Bool }
  deriving (Show, Read, Eq, Ord, Generic, Monoid, Semigroup, Typeable, ToJSON, FromJSON)

data ElemPatch a = ElemPatch_Remove
                 | ElemPatch_Insert a
                 | ElemPatch_Upsert (Patch a) (Maybe a)
  deriving (Generic, Typeable)

--TODO: Use our deriving JSON instead of the generic deriving after enabling it to handle instance contexts
instance (ToJSON a, ToJSON (Patch a)) => ToJSON (ElemPatch a)
instance (FromJSON a, FromJSON (Patch a)) => FromJSON (ElemPatch a)

deriving instance (Eq a, Eq (Patch a)) => Eq (ElemPatch a)
deriving instance (Ord a, Ord (Patch a)) => Ord (ElemPatch a)
deriving instance (Show a, Show (Patch a)) => Show (ElemPatch a)
deriving instance (Read a, Read (Patch a)) => Read (ElemPatch a)

instance Patchable a => Semigroup (ElemPatch a) where
  p' <> q' = case p' of
    (ElemPatch_Upsert p mpv) -> case q' of
      ElemPatch_Upsert q _ -> ElemPatch_Upsert (p <> q) mpv
      ElemPatch_Insert v -> ElemPatch_Insert (patch p v)
      ElemPatch_Remove -> case mpv of
        Nothing -> ElemPatch_Remove
        Just pv -> ElemPatch_Insert pv
    _ -> p'

newtype MapPatch k a = MapPatch
          { unMapPatch :: Map k (ElemPatch a)
          }
  deriving (Generic, Typeable)

emptyMapPatch :: MapPatch k a
emptyMapPatch = MapPatch Map.empty

unionMapPatch :: (Ord k, Patchable a) => MapPatch k a -> MapPatch k a -> MapPatch k a
unionMapPatch (MapPatch x) (MapPatch y) = MapPatch (Map.unionWith (<>) x y)

mapPatchInsert :: Map k a -> MapPatch k a
mapPatchInsert m = MapPatch (fmap ElemPatch_Insert m)

mapSetInsert :: k -> a -> MapPatch k (Set a)
mapSetInsert k v = MapPatch (Map.singleton k (ElemPatch_Upsert (SetPatch (Map.singleton v True)) (Just (Set.singleton v))))

mapSetDelete :: k -> a -> MapPatch k (Set a)
mapSetDelete k v = MapPatch (Map.singleton k (ElemPatch_Upsert (SetPatch (Map.singleton v False)) (Just (Set.empty))))

deriving instance (Ord k, Eq a, Eq (Patch a)) => Eq (MapPatch k a)
deriving instance (Ord k, Ord a, Ord (Patch a)) => Ord (MapPatch k a)
deriving instance (Ord k, Show k, Show a, Show (Patch a)) => Show (MapPatch k a)
deriving instance (Ord k, Read k, Read a, Read (Patch a)) => Read (MapPatch k a)
deriving instance (ToJSON k, ToJSON a, ToJSON (Patch a)) => ToJSON (MapPatch k a)
deriving instance (Ord k, FromJSON k, FromJSON a, FromJSON (Patch a)) => FromJSON (MapPatch k a)

instance (Ord k, Patchable a) => Semigroup (MapPatch k a) where
  (MapPatch mp) <> (MapPatch mq) = MapPatch $ Map.unionWith (<>) mp mq

instance (Ord k, Patchable a) => Monoid (MapPatch k a) where
  mempty = MapPatch Map.empty
  mappend = (<>)

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

elemUpsert :: Patchable a => Patch a -> a -> ElemPatch a
elemUpsert p v = ElemPatch_Upsert p (Just (patch p v))

elemUpdate :: Patchable a => Patch a -> ElemPatch a
elemUpdate p = ElemPatch_Upsert p Nothing

makeWrapped ''SetPatch
makeWrapped ''MapPatch

instance Patchable Text
