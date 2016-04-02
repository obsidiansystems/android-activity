{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, ScopedTypeVariables, EmptyDataDecls, FlexibleInstances, StandaloneDeriving, DefaultSignatures #-}

module Data.Indexed where

import Control.Lens.At
import Control.Lens (ifoldl, FoldableWithIndex(..), (.~), (&), itoList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Writer
import Data.Maybe
import Data.Proxy

data WithIndex p f v = WithIndex { _withIndex_index :: Map (Projected p (IxValue (f v))) (Set (Index (f v)))
                                 , _withIndex_data :: f v
                                 }

deriving instance (Show (f v), Show (Projected p (IxValue (f v))), Show (Index (f v))) => Show (WithIndex p f v)

instance Foldable f => Foldable (WithIndex p f) where
  foldMap f = foldMap f . _withIndex_data
  
instance (Foldable (WithIndex p f), FoldableWithIndex i f) => FoldableWithIndex i (WithIndex p f) where
  ifoldMap f = ifoldMap f . _withIndex_data

class HasIndex p f where
  {-# MINIMAL keysByIndex, index #-}
  keysByIndex :: (Ord (Index (f v)), Ord (Projected p (IxValue (f v))))
              => proxy p
              -> Set (Projected p (IxValue (f v)))
              -> f v
              -> Set (Index (f v))
  index :: (Ord (Projected p (IxValue (f v))), Ord (Index (f v)))
        => proxy p
        -> f v
        -> Map (Projected p (IxValue (f v))) (Set (Index (f v)))
  differenceByIndex :: (At (f v), Ord (Index (f v)), Ord (Projected p (IxValue (f v))), Projection p (IxValue (f v)))
                    => proxy p
                    -> Set (Projected p (IxValue (f v)))
                    -> f v
                    -> f v
  differenceByIndex p ixs f = differenceByKey (keysByIndex p ixs f) f
  intersectionByIndex :: (At (f v), Ord (Index (f v)), Ord (Projected p (IxValue (f v))), Projection p (IxValue (f v)), FoldableWithIndex (Index (f v)) f)
                      => proxy p
                      -> Set (Projected p (IxValue (f v)))
                      -> f v
                      -> f v
  intersectionByIndex p ixs f = intersectionByKey (keysByIndex p ixs f) f
  find :: (k ~ Projected p (IxValue (f v)), Ord k, Ord (Index (f v)))
       => proxy p
       -> k
       -> f v
       -> Set (Index (f v))
  find p k f = keysByIndex p (Set.singleton k) f

valuesByIndex :: ( Ord (Index (f' b))
                 , Ord (Index (f b))
                 , Ord (Projected p (IxValue (f b)))
                 , At (f' b)
                 , FoldableWithIndex (Index (f' b)) f'
                 , HasValues f f'
                 , HasIndex p f
                 , Index (f b) ~ Index (f' b)
                 )
              => proxy p
              -> f b
              -> Map (Projected p (IxValue (f b))) (f' b)
valuesByIndex p f = let ixs = index p f
                        vals = values f
                    in Map.map (\ks -> intersectionByKey ks vals) ixs

instance {-# OVERLAPPING #-} HasIndex p (WithIndex p f) where
  keysByIndex _ ixs wi = Set.unions $ Map.elems $ Map.intersection (_withIndex_index wi) $ Map.fromSet (const ()) ixs
  index _ = _withIndex_index

instance HasIndex q f => HasIndex q (WithIndex p f) where
  keysByIndex p ixs wi = keysByIndex p ixs (_withIndex_data wi)
  index p = index p . _withIndex_data

class HasValues w f | w -> f where
  values :: w a -> f a
  default values :: w a -> w a
  values = id

instance HasValues f f' => HasValues (WithIndex p f) f' where
  values = values . _withIndex_data

instance HasValues (Map k) (Map k)

withIndex :: ( FoldableWithIndex (Index (f v)) f
             , Ord (Projected p v)
             , Ord (Index (f v))
             , Projection p v
             , IxValue (f v) ~ v
             )
          => proxy p
          -> f v
          -> WithIndex p f v
withIndex p m =
  WithIndex { _withIndex_index = ifoldl (\i m' v -> Map.insertWith Set.union (project p v) (Set.singleton i) m') Map.empty m
            , _withIndex_data = m
            }

type instance Index (WithIndex p f v) = Index (f v)
type instance IxValue (WithIndex p f v) = IxValue (f v)

class Projection a v where
  type Projected a v :: *
  project :: proxy a -> v -> Projected a v

merge :: (At a, FoldableWithIndex (Index a) f) => f (Maybe (IxValue a)) -> a -> a
merge patch wi = ifoldl (\i b a -> b & at i .~ a) wi patch

differenceByKey :: (Foldable t, At a) => t (Index a) -> a -> a
differenceByKey ks wi = foldl (\b a -> b & at a .~ Nothing) wi ks

intersectionByKey :: (Ord (Index (f b)), At (f b), FoldableWithIndex (Index (f b)) f)
                 => Set (Index (f b))
                 -> f b
                 -> f b
intersectionByKey ks wi = differenceByKey (Set.difference (Set.fromList $ map fst $ itoList wi) ks) wi

nonEmptySet :: Set a -> Maybe (Set a)
nonEmptySet s = if Set.null s then Nothing else Just s

deleteFromIndex :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
deleteFromIndex k a m = Map.alter (nonEmptySet . Set.delete a . fromMaybe Set.empty) k m

addToIndex :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
addToIndex k a m = Map.alter (Just . Set.insert a . fromMaybe Set.empty) k m

instance (Ord (Index (f v)), Ixed (f v), Ord (Projected p (IxValue (f v))), Projection p (IxValue (f v))) => Ixed (WithIndex p f v) where
  ix = lensIntoWithIndexChanges ix (:[])

instance (Ord (Index (f v)), Ord (Projected p (IxValue (f v))), At (f v), Projection p (IxValue (f v))) => At (WithIndex p f v) where
  at = lensIntoWithIndexChanges at maybeToList

lensIntoWithIndexChanges :: forall k p f f' a v. (k ~ Projected p (IxValue (f v)), Projection p (IxValue (f v)), Functor f', Ord k, Ord (Index (f v)))
                         => (Index (f v) -> (a -> WriterT ([IxValue (f v)], [IxValue (f v)]) f' a) -> (f v) -> WriterT ([IxValue (f v)], [IxValue (f v)]) f' (f v))
                         -> (a -> [IxValue (f v)])
                         -> Index (f v)
                         -> (a -> f' a)
                         -> WithIndex p f v
                         -> f' (WithIndex p f v)
lensIntoWithIndexChanges op toList a f (WithIndex index' m) = (\(m', (olds, news)) -> WithIndex (foldIndex addToIndex (foldIndex deleteFromIndex index' olds) news) m') <$> runWriterT (op a (\mold -> WriterT $ (\mnew -> (mnew, (toList mold, toList mnew))) <$> f mold) m)
  where
    foldIndex f' idx xs = foldl (\i x -> f' (project (Proxy :: Proxy p) x) a i) idx xs

