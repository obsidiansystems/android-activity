{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, TemplateHaskell, DeriveGeneric, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts #-}
module Focus.AppendMap where

import Prelude hiding (map)

import Control.Arrow ((***))
import Control.Lens
import Data.Aeson
import Data.Align
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Set (Set)
import Data.Typeable
import GHC.Generics (Generic)
import Focus.Patch
import Reflex (FunctorMaybe(..))
import Reflex.Patch (Additive, Group (..))

newtype AppendMap k m = AppendMap { _unAppendMap :: Map k m }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, Functor, Foldable, Traversable, Align)

type instance (Index (AppendMap k m)) = k
type instance (IxValue (AppendMap k m)) = m

instance Ord k => Ixed (AppendMap k m) where
  ix i = _Wrapped . ix i

instance Ord k => At (AppendMap k m) where
  at i = _Wrapped . at i

instance FunctorMaybe (AppendMap k) where
  fmapMaybe f (AppendMap as) = AppendMap (Map.mapMaybe f as)

-- | Deletes a key, returning 'Nothing' if the result is empty.
nonEmptyDelete :: Ord k => k -> AppendMap k v -> Maybe (AppendMap k v)
nonEmptyDelete k vs =
  let deleted = delete k vs
  in if Focus.AppendMap.null deleted
       then Nothing
       else Just deleted

mapMaybeNoNull :: (a -> Maybe b)
               -> AppendMap token a
               -> Maybe (AppendMap token b)
mapMaybeNoNull f as =
  let bs = fmapMaybe f as
  in if Focus.AppendMap.null bs
       then Nothing
       else Just bs

instance FunctorWithIndex k (AppendMap k) where
  imap f = AppendMap . imap f . _unAppendMap
  imapped = _Wrapped . imapped

instance FoldableWithIndex k (AppendMap k) where
  ifolded = _Wrapped . ifolded

instance TraversableWithIndex k (AppendMap k) where
  itraverse = itraverseOf itraversed
  itraversed = _Wrapped . itraversed

instance (Ord k, Semigroup m) => Semigroup (AppendMap k m) where
  (AppendMap m0) <> (AppendMap m1) = AppendMap $ Map.unionWith (<>) m0 m1

instance (Ord k, Semigroup m) => Monoid (AppendMap k m) where
  mempty = AppendMap Map.empty
  mappend = (<>)

instance (ToJSON k, ToJSON m) => ToJSON (AppendMap k m) where
  toJSON = toJSON . Map.toList . _unAppendMap

instance (FromJSON k, FromJSON m, Ord k) => FromJSON (AppendMap k m) where
  parseJSON r = do
    res <- parseJSON r
    fmap AppendMap . sequence . Map.fromListWithKey (fail "duplicate key in JSON deserialization of AppendMap") . fmap (fmap return) $ res

instance (Ord k, Group q) => Group (AppendMap k q) where
  negateG = map negateG

instance (Ord k, Additive q) => Additive (AppendMap k q)

(!) :: Ord k => AppendMap k a -> k -> a
(!) m k = (Map.!) (_unAppendMap m) k
infixl 9 !

(\\) :: Ord k => AppendMap k a -> AppendMap k b -> AppendMap k a
(\\) ma mb = AppendMap $ (Map.\\) (_unAppendMap ma) (_unAppendMap mb)
infixl 9 \\

null :: AppendMap k a -> Bool
null = Map.null . _unAppendMap

size :: AppendMap k a -> Int
size = Map.size . _unAppendMap

member :: Ord k => k -> AppendMap k a -> Bool
member k = Map.member k . _unAppendMap

notMember :: Ord k => k -> AppendMap k a -> Bool
notMember k = Map.notMember k . _unAppendMap

lookup :: Ord k => k -> AppendMap k a -> Maybe a
lookup k = Map.lookup k . _unAppendMap

findWithDefault :: Ord k => a -> k -> AppendMap k a -> a
findWithDefault a k = Map.findWithDefault a k . _unAppendMap

lookupLT :: Ord k => k -> AppendMap k v -> Maybe (k, v)
lookupLT k = Map.lookupLT k . _unAppendMap

lookupGT :: Ord k => k -> AppendMap k v -> Maybe (k, v)
lookupGT k = Map.lookupGT k . _unAppendMap

lookupLE :: Ord k => k -> AppendMap k v -> Maybe (k, v)
lookupLE k = Map.lookupLE k . _unAppendMap

lookupGE :: Ord k => k -> AppendMap k v -> Maybe (k, v)
lookupGE k = Map.lookupGE k . _unAppendMap

empty :: AppendMap k a
empty = AppendMap Map.empty

singleton :: k -> a -> AppendMap k a
singleton k a = AppendMap $ Map.singleton k a

insert :: Ord k => k -> a -> AppendMap k a -> AppendMap k a
insert k a = AppendMap . Map.insert k a . _unAppendMap

insertWith :: Ord k => (a -> a -> a) -> k -> a -> AppendMap k a -> AppendMap k a
insertWith f k a = AppendMap . Map.insertWith f k a . _unAppendMap

insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> AppendMap k a -> AppendMap k a
insertWithKey f k a = AppendMap . Map.insertWithKey f k a . _unAppendMap

insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> AppendMap k a -> (Maybe a, AppendMap k a)
insertLookupWithKey f k a = fmap AppendMap . Map.insertLookupWithKey f k a . _unAppendMap

delete :: Ord k => k -> AppendMap k a -> AppendMap k a
delete k = AppendMap . Map.delete k . _unAppendMap

adjust :: Ord k => (a -> a) -> k -> AppendMap k a -> AppendMap k a
adjust f k = AppendMap . Map.adjust f k . _unAppendMap

adjustWithKey :: Ord k => (k -> a -> a) -> k -> AppendMap k a -> AppendMap k a
adjustWithKey f k = AppendMap . Map.adjustWithKey f k . _unAppendMap

update :: Ord k => (a -> Maybe a) -> k -> AppendMap k a -> AppendMap k a
update f k = AppendMap . Map.update f k . _unAppendMap

updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> AppendMap k a -> AppendMap k a
updateWithKey f k = AppendMap . Map.updateWithKey f k . _unAppendMap

updateLookupWithKey :: Ord k => (k -> a -> Maybe a) -> k -> AppendMap k a -> (Maybe a, AppendMap k a)
updateLookupWithKey f k = fmap AppendMap . Map.updateLookupWithKey f k . _unAppendMap

alter :: Ord k => (Maybe a -> Maybe a) -> k -> AppendMap k a -> AppendMap k a
alter f k = AppendMap . Map.alter f k . _unAppendMap

union :: Ord k => AppendMap k a -> AppendMap k a -> AppendMap k a
union ma mb = AppendMap $ Map.union (_unAppendMap ma) (_unAppendMap mb)

unionWith :: Ord k => (a -> a -> a) -> AppendMap k a -> AppendMap k a -> AppendMap k a
unionWith f ma mb = AppendMap $ Map.unionWith f (_unAppendMap ma) (_unAppendMap mb)

unionWithKey :: Ord k => (k -> a -> a -> a) -> AppendMap k a -> AppendMap k a -> AppendMap k a
unionWithKey f ma mb = AppendMap $ Map.unionWithKey f (_unAppendMap ma) (_unAppendMap mb)

unions :: Ord k => [AppendMap k a] -> AppendMap k a
unions = AppendMap . Map.unions . fmap _unAppendMap

unionsWith :: Ord k => (a -> a -> a) -> [AppendMap k a] -> AppendMap k a
unionsWith f = AppendMap . Map.unionsWith f . fmap _unAppendMap

difference :: Ord k => AppendMap k a -> AppendMap k b -> AppendMap k a
difference = (\\)

differenceWith :: Ord k => (a -> b -> Maybe a) -> AppendMap k a -> AppendMap k b -> AppendMap k a
differenceWith f ma mb = AppendMap $ Map.differenceWith f (_unAppendMap ma) (_unAppendMap mb)

differenceWithKey :: Ord k => (k -> a -> b -> Maybe a) -> AppendMap k a -> AppendMap k b -> AppendMap k a
differenceWithKey f ma mb = AppendMap $ Map.differenceWithKey f (_unAppendMap ma) (_unAppendMap mb)

intersection :: Ord k => AppendMap k a -> AppendMap k b -> AppendMap k a
intersection ma mb = AppendMap $ Map.intersection (_unAppendMap ma) (_unAppendMap mb)

intersectionWith :: Ord k => (a -> b -> c) -> AppendMap k a -> AppendMap k b -> AppendMap k c
intersectionWith f ma mb = AppendMap $ Map.intersectionWith f (_unAppendMap ma) (_unAppendMap mb)

intersectionWithKey :: Ord k => (k -> a -> b -> c) -> AppendMap k a -> AppendMap k b -> AppendMap k c
intersectionWithKey f ma mb = AppendMap $ Map.intersectionWithKey f (_unAppendMap ma) (_unAppendMap mb)

mergeWithKey :: Ord k => (k -> a -> b -> Maybe c) -> (AppendMap k a -> AppendMap k c) -> (AppendMap k b -> AppendMap k c) -> AppendMap k a -> AppendMap k b -> AppendMap k c
mergeWithKey f fa fb ma mb = AppendMap $ Map.mergeWithKey f (_unAppendMap . fa . AppendMap) (_unAppendMap . fb . AppendMap) (_unAppendMap ma) (_unAppendMap mb)

map :: (a -> b) -> AppendMap k a -> AppendMap k b
map = fmap

mapWithKey :: (k -> a -> b) -> AppendMap k a -> AppendMap k b
mapWithKey = imap

traverseWithKey :: Applicative t => (k -> a -> t b) -> AppendMap k a -> t (AppendMap k b)
traverseWithKey = itraverse

mapAccum :: (a -> b -> (a, c)) -> a -> AppendMap k b -> (a, AppendMap k c)
mapAccum f a = fmap AppendMap . Map.mapAccum f a . _unAppendMap

mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> AppendMap k b -> (a, AppendMap k c)
mapAccumWithKey f a = fmap AppendMap . Map.mapAccumWithKey f a . _unAppendMap

mapAccumRWithKey :: (a -> k -> b -> (a, c)) -> a -> AppendMap k b -> (a, AppendMap k c)
mapAccumRWithKey f a = fmap AppendMap . Map.mapAccumRWithKey f a . _unAppendMap

mapKeys :: Ord k2 => (k1 -> k2) -> AppendMap k1 a -> AppendMap k2 a
mapKeys f = AppendMap . Map.mapKeys f . _unAppendMap

mapKeysWith :: Ord k2 => (a -> a -> a) -> (k1 -> k2) -> AppendMap k1 a -> AppendMap k2 a
mapKeysWith f fk = AppendMap . Map.mapKeysWith f fk . _unAppendMap

mapKeysMonotonic :: (k1 -> k2) -> AppendMap k1 a -> AppendMap k2 a
mapKeysMonotonic f = AppendMap . Map.mapKeysMonotonic f . _unAppendMap

foldr :: (a -> b -> b) -> b -> AppendMap k a -> b
foldr f b = Map.foldr f b . _unAppendMap

foldl :: (a -> b -> a) -> a -> AppendMap k b -> a
foldl f a = Map.foldl f a . _unAppendMap

foldrWithKey :: (k -> a -> b -> b) -> b -> AppendMap k a -> b
foldrWithKey f b = Map.foldrWithKey f b . _unAppendMap

foldlWithKey :: (a -> k -> b -> a) -> a -> AppendMap k b -> a
foldlWithKey f a = Map.foldlWithKey f a . _unAppendMap

foldMapWithKey :: Monoid m => (k -> a -> m) -> AppendMap k a -> m
foldMapWithKey f = Map.foldMapWithKey f . _unAppendMap

foldr' :: (a -> b -> b) -> b -> AppendMap k a -> b
foldr' f b = Map.foldr' f b . _unAppendMap

foldl' :: (a -> b -> a) -> a -> AppendMap k b -> a
foldl' f a = Map.foldl' f a . _unAppendMap

foldrWithKey' :: (k -> a -> b -> b) -> b -> AppendMap k a -> b
foldrWithKey' f b = Map.foldrWithKey' f b . _unAppendMap

foldlWithKey' :: (a -> k -> b -> a) -> a -> AppendMap k b -> a
foldlWithKey' f a = Map.foldlWithKey'  f a . _unAppendMap

elems :: AppendMap k a -> [a]
elems = Map.elems . _unAppendMap

keys :: AppendMap k a -> [k]
keys = Map.keys . _unAppendMap

assocs :: AppendMap k a -> [(k, a)]
assocs = Map.assocs . _unAppendMap

keysSet :: AppendMap k a -> Set k
keysSet = Map.keysSet . _unAppendMap

fromSet :: (k -> a) -> Set k -> AppendMap k a
fromSet f = AppendMap . Map.fromSet f

toList :: AppendMap k a -> [(k, a)]
toList = Map.toList . _unAppendMap

fromList :: Ord k => [(k, a)] -> AppendMap k a
fromList = AppendMap . Map.fromList

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> AppendMap k a
fromListWith f = AppendMap . Map.fromListWith f

fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k, a)] -> AppendMap k a
fromListWithKey f = AppendMap . Map.fromListWithKey f

toAscList :: AppendMap k a -> [(k, a)]
toAscList = Map.toAscList . _unAppendMap

toDescList :: AppendMap k a -> [(k, a)]
toDescList = Map.toDescList . _unAppendMap

fromAscList :: Eq k => [(k, a)] -> AppendMap k a
fromAscList = AppendMap . Map.fromAscList

fromAscListWith :: Eq k => (a -> a -> a) -> [(k, a)] -> AppendMap k a
fromAscListWith f = AppendMap . Map.fromAscListWith f

fromAscListWithKey :: Eq k => (k -> a -> a -> a) -> [(k, a)] -> AppendMap k a
fromAscListWithKey f = AppendMap . Map.fromAscListWithKey f

fromDistinctAscList :: [(k, a)] -> AppendMap k a
fromDistinctAscList = AppendMap . Map.fromDistinctAscList

filter :: (a -> Bool) -> AppendMap k a -> AppendMap k a
filter f = AppendMap . Map.filter f . _unAppendMap

filterWithKey :: (k -> a -> Bool) -> AppendMap k a -> AppendMap k a
filterWithKey f = AppendMap . Map.filterWithKey f . _unAppendMap

partition :: (a -> Bool) -> AppendMap k a -> (AppendMap k a, AppendMap k a)
partition f = over both AppendMap . Map.partition f . _unAppendMap

partitionWithKey :: (k -> a -> Bool) -> AppendMap k a -> (AppendMap k a, AppendMap k a)
partitionWithKey f = over both AppendMap . Map.partitionWithKey f . _unAppendMap

mapMaybe :: (a -> Maybe b) -> AppendMap k a -> AppendMap k b
mapMaybe f = AppendMap . Map.mapMaybe f . _unAppendMap

mapMaybeWithKey :: (k -> a -> Maybe b) -> AppendMap k a -> AppendMap k b
mapMaybeWithKey f = AppendMap . Map.mapMaybeWithKey f . _unAppendMap

mapEither :: (a -> Either b c) -> AppendMap k a -> (AppendMap k b, AppendMap k c)
mapEither f = (AppendMap *** AppendMap) . Map.mapEither f . _unAppendMap

mapEitherWithKey :: (k -> a -> Either b c) -> AppendMap k a -> (AppendMap k b, AppendMap k c)
mapEitherWithKey f = (AppendMap *** AppendMap) . Map.mapEitherWithKey f . _unAppendMap

split :: Ord k => k -> AppendMap k a -> (AppendMap k a, AppendMap k a)
split k = over both AppendMap . Map.split k . _unAppendMap

splitLookup :: Ord k => k -> AppendMap k a -> (AppendMap k a, Maybe a, AppendMap k a)
splitLookup k = (\(m, a, m') -> (AppendMap m, a, AppendMap m')) . Map.splitLookup k . _unAppendMap

splitRoot :: AppendMap k b -> [AppendMap k b]
splitRoot = fmap AppendMap . Map.splitRoot . _unAppendMap

isSubmapOf :: (Ord k, Eq a) => AppendMap k a -> AppendMap k a -> Bool
isSubmapOf ma = Map.isSubmapOf (_unAppendMap ma) . _unAppendMap

isSubmapOfBy :: Ord k => (a -> b -> Bool) -> AppendMap k a -> AppendMap k b -> Bool
isSubmapOfBy f ma = Map.isSubmapOfBy f (_unAppendMap ma) . _unAppendMap

isProperSubmapOf :: (Ord k, Eq a) => AppendMap k a -> AppendMap k a -> Bool
isProperSubmapOf ma = Map.isProperSubmapOf (_unAppendMap ma) . _unAppendMap

isProperSubmapOfBy :: Ord k => (a -> b -> Bool) -> AppendMap k a -> AppendMap k b -> Bool
isProperSubmapOfBy f ma = Map.isProperSubmapOfBy f (_unAppendMap ma) . _unAppendMap

lookupIndex :: Ord k => k -> AppendMap k a -> Maybe Int
lookupIndex k = Map.lookupIndex k . _unAppendMap

findIndex :: Ord k => k -> AppendMap k a -> Int
findIndex k = Map.findIndex k . _unAppendMap

elemAt :: Int -> AppendMap k a -> (k, a)
elemAt n = Map.elemAt n . _unAppendMap

updateAt :: (k -> a -> Maybe a) -> Int -> AppendMap k a -> AppendMap k a
updateAt f n = AppendMap . Map.updateAt f n . _unAppendMap

deleteAt :: Int -> AppendMap k a -> AppendMap k a
deleteAt n = AppendMap . Map.deleteAt n . _unAppendMap

findMin :: AppendMap k a -> (k, a)
findMin = Map.findMin . _unAppendMap

findMax :: AppendMap k a -> (k, a)
findMax = Map.findMax . _unAppendMap

deleteMin :: AppendMap k a -> AppendMap k a
deleteMin = AppendMap . Map.deleteMin . _unAppendMap

deleteMax :: AppendMap k a -> AppendMap k a
deleteMax = AppendMap . Map.deleteMax . _unAppendMap

deleteFindMin :: AppendMap k a -> ((k, a), AppendMap k a)
deleteFindMin = fmap AppendMap . Map.deleteFindMin . _unAppendMap

deleteFindMax :: AppendMap k a -> ((k, a), AppendMap k a)
deleteFindMax = fmap AppendMap . Map.deleteFindMax . _unAppendMap

updateMin :: (a -> Maybe a) -> AppendMap k a -> AppendMap k a
updateMin f = AppendMap . Map.updateMin f . _unAppendMap

updateMax :: (a -> Maybe a) -> AppendMap k a -> AppendMap k a
updateMax f = AppendMap . Map.updateMax f . _unAppendMap

updateMinWithKey :: (k -> a -> Maybe a) -> AppendMap k a -> AppendMap k a
updateMinWithKey f = AppendMap . Map.updateMinWithKey f . _unAppendMap

updateMaxWithKey :: (k -> a -> Maybe a) -> AppendMap k a -> AppendMap k a
updateMaxWithKey f = AppendMap . Map.updateMaxWithKey f . _unAppendMap

minView :: AppendMap k a -> Maybe (a, AppendMap k a)
minView = fmap (fmap AppendMap) . Map.minView . _unAppendMap

maxView :: AppendMap k a -> Maybe (a, AppendMap k a)
maxView = fmap (fmap AppendMap) . Map.maxView . _unAppendMap

minViewWithKey :: AppendMap k a -> Maybe ((k, a), AppendMap k a)
minViewWithKey = fmap (fmap AppendMap) . Map.minViewWithKey . _unAppendMap

maxViewWithKey :: AppendMap k a -> Maybe ((k, a), AppendMap k a)
maxViewWithKey = fmap (fmap AppendMap) . Map.maxViewWithKey . _unAppendMap

showTree :: (Show k, Show a) => AppendMap k a -> String
showTree = Map.showTree . _unAppendMap

showTreeWith :: (k -> a -> String) -> Bool -> Bool -> AppendMap k a -> String
showTreeWith f b b' = Map.showTreeWith f b b' . _unAppendMap

valid :: Ord k => AppendMap k a -> Bool
valid = Map.valid . _unAppendMap

instance Default (AppendMap k v) where
  def = empty

newtype AppendMapPatch k a = AppendMapPatch
          { unAppendMapPatch :: AppendMap k (ElemPatch a)
          }
  deriving (Generic, Typeable)

instance (Ord k, Patchable a) => Semigroup (AppendMapPatch k a) where
  (AppendMapPatch mp) <> (AppendMapPatch mq) = AppendMapPatch $ (<>) mp mq

instance (Ord k, Patchable a) => Monoid (AppendMapPatch k a) where
  mempty = AppendMapPatch empty
  mappend = (<>)

instance (Ord k, Patchable v) => Patchable (AppendMap k v) where
  type Patch (AppendMap k v) = AppendMapPatch k v
  patch (AppendMapPatch (AppendMap p)) (AppendMap m) = AppendMap $ patch (MapPatch p) m

(~:) :: (Default v, Patchable v) => k -> Patch v -> Patch (AppendMap k v)
k ~: p = AppendMapPatch (singleton k (ElemPatch_Upsert p (Just (patch p def))))
infixr 5 ~:

appendMapInsert :: AppendMap k a -> AppendMapPatch k a
appendMapInsert = AppendMapPatch . fmap ElemPatch_Insert

deriving instance (Ord k, Eq a, Eq (Patch a)) => Eq (AppendMapPatch k a)
deriving instance (Ord k, Ord a, Ord (Patch a)) => Ord (AppendMapPatch k a)
deriving instance (Ord k, Show k, Show a, Show (Patch a)) => Show (AppendMapPatch k a)
deriving instance (Ord k, Read k, Read a, Read (Patch a)) => Read (AppendMapPatch k a)
deriving instance (ToJSON k, ToJSON a, ToJSON (Patch a)) => ToJSON (AppendMapPatch k a)
deriving instance (Ord k, FromJSON k, FromJSON a, FromJSON (Patch a)) => FromJSON (AppendMapPatch k a)

makeWrapped ''AppendMap

-- | Operator for creating a singleton 'Map'
(=:) :: k -> a -> AppendMap k a
k =: v = singleton k v
infixr 7 =:

