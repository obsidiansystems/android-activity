{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, ScopedTypeVariables, EmptyDataDecls, FlexibleInstances, StandaloneDeriving #-}

module Data.Indexed ( WithIndex
                    , withIndex
                    , find
                    , merge
                    , values
                    , differenceKeys
                    , intersectionKeys
                    , differenceIndices
                    , intersectionIndices
                    , Projection(..)
                    )  where

import Control.Lens.At
import Control.Lens (ifoldl, FoldableWithIndex, (.~), (&), itoList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Writer
import Data.Maybe
import Data.Proxy

data WithIndex p m = WithIndex { _withIndex_index :: Map (Projected p (IxValue m)) (Set (Index m))
                               , _withIndex_data :: m
                               }

deriving instance (Show m, Show (Projected p (IxValue m)), Show (Index m)) => Show (WithIndex p m)

withIndex :: ( FoldableWithIndex (Index (f v)) f
             , Ord (Projected p v)
             , Ord (Index (f v))
             , Projection p v
             , IxValue (f v) ~ v
             )
          => proxy p
          -> f v
          -> WithIndex p (f v)
withIndex p m =
  WithIndex { _withIndex_index = ifoldl (\i m' v -> Map.insertWith Set.union (project p v) (Set.singleton i) m') Map.empty m
            , _withIndex_data = m
            }

type instance Index (WithIndex p m) = Index m
type instance IxValue (WithIndex p m) = IxValue m

class Projection a v where
  type Projected a v :: *
  project :: proxy a -> v -> Projected a v

find :: (k ~ Projected p (IxValue m), Ord k) => k -> WithIndex p m -> Set (Index m)
find k i = fromMaybe Set.empty $ Map.lookup k $ _withIndex_index i

merge :: (At a, FoldableWithIndex (Index a) f) => a -> f (Maybe (IxValue a)) -> a
merge wi patch = ifoldl (\i b a -> b & at i .~ a) wi patch

differenceKeys :: (Foldable t, At a) => a -> t (Index a) -> a
differenceKeys wi ks = foldl (\b a -> b & at a .~ Nothing) wi ks

intersectionKeys ::( Ord (Index (f b))
                   , Ord (Projected p (IxValue (f b)))
                   , At (f b)
                   , FoldableWithIndex (Index (f b)) f
                   , Projection p (IxValue (f b))
                   )
                 => WithIndex p (f b)
                 -> Set (Index (f b))
                 -> WithIndex p (f b)
intersectionKeys wi ks = differenceKeys wi $ Set.difference (Set.fromList $ map fst $ itoList $ _withIndex_data wi) ks

differenceIndices :: ( At m
                     , Ord (Index m)
                     , Ord (Projected p (IxValue m))
                     , Projection p (IxValue m)
                     )
                  => WithIndex p m -> Set (Projected p (IxValue m)) -> WithIndex p m
differenceIndices wi ixs = differenceKeys wi $ intersectingKeysFromIndices wi ixs

intersectionIndices :: ( FoldableWithIndex (Index (f b)) f
                       , At (f b)
                       , Ord (Index (f b))
                       , Projection p (IxValue (f b))
                       , Ord (Projected p (IxValue (f b)))
                       )
                    => WithIndex p (f b)
                    -> Set (Projected p (IxValue (f b)))
                    -> WithIndex p (f b)
intersectionIndices wi ixs = intersectionKeys wi $ intersectingKeysFromIndices wi ixs

intersectingKeysFromIndices :: ( Ord (Index m)
                               , Ord (Projected p (IxValue m))
                               )
                            => WithIndex p m
                            -> Set (Projected p (IxValue m))
                            -> Set (Index m)
intersectingKeysFromIndices wi ixs = Set.unions $ Map.elems $ Map.intersection (_withIndex_index wi) $ Map.fromSet (const ()) ixs

values :: WithIndex p m -> m
values = _withIndex_data

nonEmptySet :: Set a -> Maybe (Set a)
nonEmptySet s = if Set.null s then Nothing else Just s

deleteFromIndex :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
deleteFromIndex k a m = Map.alter (nonEmptySet . Set.delete a . fromMaybe Set.empty) k m

addToIndex :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
addToIndex k a m = Map.alter (Just . Set.insert a . fromMaybe Set.empty) k m

instance (Ord (Index m), Ixed m, Ord (Projected p (IxValue m)), Projection p (IxValue m)) => Ixed (WithIndex p m) where
  ix = lensIntoWithIndexChanges ix (:[])

instance (Ord (Index m), Ord (Projected p (IxValue m)), At m, Projection p (IxValue m)) => At (WithIndex p m) where
  at = lensIntoWithIndexChanges at maybeToList

lensIntoWithIndexChanges :: forall k p m f a. (k ~ Projected p (IxValue m), Projection p (IxValue m), Functor f, Ord k, Ord (Index m))
                         => (Index m -> (a -> WriterT ([IxValue m], [IxValue m]) f a) -> m -> WriterT ([IxValue m], [IxValue m]) f m)
                         -> (a -> [IxValue m])
                         -> Index m
                         -> (a -> f a)
                         -> WithIndex p m
                         -> f (WithIndex p m)
lensIntoWithIndexChanges op toList a f (WithIndex index m) = (\(m', (olds, news)) -> WithIndex (foldIndex addToIndex (foldIndex deleteFromIndex index olds) news) m') <$> runWriterT (op a (\mold -> WriterT $ (\mnew -> (mnew, (toList mold, toList mnew))) <$> f mold) m)
  where
    foldIndex f' idx xs = foldl (\i x -> f' (project (Proxy :: Proxy p) x) a i) idx xs

