{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DefaultSignatures, EmptyDataDecls, MultiParamTypeClasses, TypeFamilies, DeriveGeneric, FlexibleInstances, FlexibleContexts, FunctionalDependencies, StandaloneDeriving, UndecidableInstances, LambdaCase #-}
module Focus.Patch where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable
import GHC.Generics

import Focus.Schema

class Semigroup (Patch v) => Patchable v where
  type Patch v :: *
  type instance Patch v = First v
  -- patch should be a left {semigroup, monoid} action if the Patch type is one
  patch :: Patch v -> v -> v
  default patch :: First v -> v -> v
  patch (First v) _ = v

data PairPatch a b = PatchFst (Patch a)
                   | PatchSnd (Patch b)
                   | PatchBoth (Patch a) (Patch b)
  deriving (Generic, Typeable)

deriving instance (Show (Patch a), Show (Patch b)) => Show (PairPatch a b)
deriving instance (Read (Patch a), Read (Patch b)) => Read (PairPatch a b)
deriving instance (Eq (Patch a), Eq (Patch b)) => Eq (PairPatch a b)
deriving instance (Ord (Patch a), Ord (Patch b)) => Ord (PairPatch a b)

instance (Patchable a, Patchable b) => Semigroup (PairPatch a b) where
  (PatchFst a) <> (PatchFst a') = PatchFst (a <> a')
  (PatchFst a) <> (PatchSnd b) = PatchBoth a b
  (PatchFst a) <> (PatchBoth a' b) = PatchBoth (a <> a') b
  (PatchSnd b) <> (PatchFst a) = PatchBoth a b
  (PatchSnd b) <> (PatchSnd b') = PatchSnd (b <> b')
  (PatchSnd b) <> (PatchBoth a b') = PatchBoth a (b <> b')
  (PatchBoth a b) <> (PatchFst a') = PatchBoth (a <> a') b
  (PatchBoth a b) <> (PatchSnd b') = PatchBoth a (b <> b')
  (PatchBoth a b) <> (PatchBoth a' b') = PatchBoth (a <> a') (b <> b')

instance (Patchable a, Patchable b) => Patchable (a, b) where
  type Patch (a, b) = PairPatch a b
  patch p (x, y) = case p of
    PatchFst px -> (patch px x, y)
    PatchSnd py -> (x, patch py y)
    PatchBoth px py -> (patch px x, patch py y)

instance (FromJSON (Patch a), FromJSON (Patch b)) => FromJSON (PairPatch a b)
instance (ToJSON (Patch a), ToJSON (Patch b)) => ToJSON (PairPatch a b)

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
  deriving (Show, Read, Eq, Ord, Generic, Monoid, Semigroup, Typeable)

setPatchInsert :: a -> SetPatch a
setPatchInsert v = SetPatch (Map.singleton v True)

setPatchDelete :: a -> SetPatch a
setPatchDelete a = SetPatch (Map.singleton a False)

setPatchAdd :: Set a -> SetPatch a
setPatchAdd s = SetPatch (Map.fromSet (const True) s)

setPatchRemove :: Set a -> SetPatch a
setPatchRemove s = SetPatch (Map.fromSet (const False) s)

instance (Ord a, FromJSON a) => FromJSON (SetPatch a) where
  parseJSON v = SetPatch <$> parseJSONMap v

instance ToJSON a => ToJSON (SetPatch a) where
  toJSON = toJSONMap . unSetPatch

data ElemPatch a = ElemPatch_Remove -- ^ Remove a value.
                 | ElemPatch_Insert a -- ^ Simply insert a value, replacing any previous one.
                 | ElemPatch_Upsert (Patch a) (Maybe a) -- ^ A patch to be applied only in the case where there is an existing value,
                                                        -- together with an initial value in case none is present
                                                        -- (the initial value will not have the patch applied to it).
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
      ElemPatch_Upsert q mqv -> ElemPatch_Upsert (p <> q) (fmap (patch p) mqv <|> mpv)
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

(~:) :: (Default v, Patchable v) => k -> Patch v -> Patch (Map k v)
k ~: p = MapPatch (Map.singleton k (ElemPatch_Upsert p (Just (patch p def))))
infixr 5 ~:

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

elemUpsert :: Patchable a => Patch a -> a -> ElemPatch a
elemUpsert p v = ElemPatch_Upsert p (Just (patch p v))

elemUpdate :: Patch a -> ElemPatch a
elemUpdate p = ElemPatch_Upsert p Nothing

makeWrapped ''SetPatch
makeWrapped ''MapPatch

instance Patchable Text
