{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, ScopedTypeVariables, EmptyDataDecls, FlexibleInstances, StandaloneDeriving #-}

module Data.Indexed where

import Control.Lens.At
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Writer
import Data.Maybe
import Data.Proxy

data WithIndex p m = WithIndex { _withIndex_projection :: Proxy p
                               , _withIndex_index :: Map (Projected p (IxValue m)) (Set (Index m))
                               , _withIndex_data :: m
                               }

deriving instance (Show m, Show (Projected p (IxValue m)), Show (Index m)) => Show (WithIndex p m)

type instance Index (WithIndex p m) = Index m
type instance IxValue (WithIndex p m) = IxValue m

class Projection a v where
  type Projected a v :: *
  project :: Proxy a -> v -> Projected a v

find :: (k ~ Projected p (IxValue m), Ord k) => k -> WithIndex p m -> Set (Index m)
find k i = fromMaybe Set.empty $ Map.lookup k $ _withIndex_index i

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
lensIntoWithIndexChanges op toList a f (WithIndex p index m) = (\(m', (olds, news)) -> WithIndex p (foldIndex addToIndex (foldIndex deleteFromIndex index olds) news) m') <$> runWriterT (op a (\mold -> WriterT $ (\mnew -> (mnew, (toList mold, toList mnew))) <$> f mold) m)
  where
    foldIndex f' idx xs = foldl (\i x -> f' (project p x) a i) idx xs

