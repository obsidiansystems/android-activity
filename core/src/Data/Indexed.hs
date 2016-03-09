{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, ScopedTypeVariables, EmptyDataDecls, FlexibleInstances, StandaloneDeriving #-}

module Data.Indexed where

import Control.Lens.At
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Writer
import Data.Maybe
import Control.Lens hiding (Indexed)
import Data.Proxy

data Indexed p m = Indexed { _indexed_index :: Map (Projected p (IxValue m)) (Set (Index m))
                           , _indexed_data :: m
                           }

deriving instance (Show m, Show (Projected p (IxValue m)), Show (Index m)) => Show (Indexed p m)

type instance Index (Indexed p m) = Index m
type instance IxValue (Indexed p m) = IxValue m

class Projection a v where
  type Projected a v :: *
  project :: Proxy a -> v -> Projected a v

find :: (k ~ Projected p (IxValue m), Ord k) => k -> Indexed p m -> Set (Index m)
find k i = fromMaybe Set.empty $ Map.lookup k $ _indexed_index i

nonEmptySet :: Set a -> Maybe (Set a)
nonEmptySet s = if Set.null s then Nothing else Just s

deleteFromIndex :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
deleteFromIndex k a m = Map.alter (nonEmptySet . Set.delete a . fromMaybe Set.empty) k m

addToIndex :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
addToIndex k a m = Map.alter (Just . Set.insert a . fromMaybe Set.empty) k m

instance (Ord (Index m), Ixed m, Ord (Projected p (IxValue m)), Projection p (IxValue m)) => Ixed (Indexed p m) where
  ix = lensIntoWithIndexChanges ix (:[])

instance (Ord (Index m), Ord (Projected p (IxValue m)), At m, Projection p (IxValue m)) => At (Indexed p m) where
  at = lensIntoWithIndexChanges at maybeToList

lensIntoWithIndexChanges :: forall k p m f a. (k ~ Projected p (IxValue m), Projection p (IxValue m), Functor f, Ord k, Ord (Index m))
                         => (Index m -> (a -> WriterT ([IxValue m], [IxValue m]) f a) -> m -> WriterT ([IxValue m], [IxValue m]) f m)
                         -> (a -> [IxValue m])
                         -> Index m
                         -> (a -> f a)
                         -> Indexed p m
                         -> f (Indexed p m)
lensIntoWithIndexChanges op toList a f (Indexed index m) = (\(m', (olds, news)) -> Indexed (foldIndex addToIndex (foldIndex deleteFromIndex index olds) news) m') <$> runWriterT (op a (\mold -> WriterT $ (\mnew -> (mnew, (toList mold, toList mnew))) <$> f mold) m)
  where
    foldIndex f index xs = foldl (\i x -> f (project (Proxy :: Proxy p) x) a i) index xs

data Length

instance Projection Length String where
  type Projected Length String = Int
  project _ = length

test = (Indexed Map.empty Map.empty :: Indexed Length (Map Int String))
  & at 5 .~ Just ("Hello, world!" :: String)
  & at 6 .~ Just "asdfasdfasdfa"
  & at 1 .~ Just "a"
