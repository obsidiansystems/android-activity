{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, ScopedTypeVariables, EmptyDataDecls, FlexibleInstances, StandaloneDeriving, DefaultSignatures, LambdaCase #-}

module Data.Indexed where

import Control.Lens.At
import Control.Lens (ifoldl, FoldableWithIndex(..), (.~), (%~), (&), itoList)
import Control.Monad.Writer
import Data.Aeson
import Data.Foldable hiding (find, toList)
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Focus.AppendMap (AppendMap)
import qualified Focus.AppendMap as Map

-- | This type adds a secondary index to a data structure. The first type parameter p is a phantom used to indicate the instance of 'Projection' to use
-- in order to obtain keys for the additional index. The second type parameter f is intended to be some FoldableWithIndex data structure, such as some Map k or another
-- application of WithIndex. The third type parameter v is the type of values in the underlying Map structure which we're going to be projecting the new indices from.
data WithIndex p f v = WithIndex { _withIndex_index :: AppendMap (Projected p (IxValue (f v))) (Set (Index (f v)))
                                 , _withIndex_data :: f v
                                 }

deriving instance (Show (f v), Show (Projected p (IxValue (f v))), Show (Index (f v))) => Show (WithIndex p f v)
deriving instance (Read (f v), Read (Projected p (IxValue (f v))), Ord (Projected p (IxValue (f v))), Ord (Index (f v)), Read (Index (f v))) => Read (WithIndex p f v)

instance (FromJSON (f v), IxValue (f v) ~ v, Ord (Index (f v)), Ord (Projected p v), FoldableWithIndex (Index (f v)) f, Projection p v) => FromJSON (WithIndex p f v) where
  parseJSON v = withIndex (Proxy :: Proxy p) <$> parseJSON v

instance (ToJSON (f v)) => ToJSON (WithIndex p f v) where
  toJSON = toJSON . _withIndex_data

-- | WARNING: The mappend here mappends each value in the underlying data, even if this is not what the underlying Monoid instance would do
--   WARNING: The mappend here will entirely rebuild the index of the left argument, so it must ALWAYS be invoked with the smaller argument on the left
instance (IxValue (f v) ~ v, Monoid v, Ord (Index (f v)), Ord (Projected p v), FoldableWithIndex (Index (f v)) f, Projection p v, Monoid (f v), At (f v)) => Monoid (WithIndex p f v) where
  mempty = withIndex (Proxy :: Proxy p) mempty
  mappend w w' = merge' f w w'
    where
      f b = \case
        Nothing -> Just b
        Just a -> Just (b <> a)

{-
merge :: (At a, FoldableWithIndex (Index a) f) => f (Maybe (IxValue a)) -> a -> a
merge patch wi = ifoldl (\i b a -> b & at i .~ a) wi patch
-}

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
        -> AppendMap (Projected p (IxValue (f v))) (Set (Index (f v)))
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
  findValues :: (Ord (Projected p (IxValue (f b))), Ord (Index (f b)), FoldableWithIndex (Index (f b)) f, At (f b))
             => proxy p
             -> Projected p (IxValue (f b))
             -> f b
             -> f b
  findValues p k f = intersectionByKey (find p k f) f

-- | Filter for the entries whose corresponding index is greater than or equal to a specified value.
findValuesGEq :: ( Ord (Projected p (IxValue (f b)))
                 , Ord (Index (f b))
                 , FoldableWithIndex (Index (f b)) f
                 , At (f b)
                 , HasIndex p f)
              => proxy p
              -> Projected p (IxValue (f b))
              -> f b
              -> f b
findValuesGEq p i f =
  let idx = index p f
      (_, mx, greater) = Map.splitLookup i idx
      withExactMatch = case mx of Nothing -> id; Just s -> Set.union s
  in intersectionByKey (withExactMatch (fold greater)) f

valuesByIndex :: ( Ord (Index (f' b))
                 , Ord (Projected p (IxValue (f b)))
                 , At (f' b)
                 , FoldableWithIndex (Index (f' b)) f'
                 , HasValues f f'
                 , HasIndex p f
                 , Index (f b) ~ Index (f' b)
                 )
              => proxy p
              -> f b
              -> AppendMap (Projected p (IxValue (f b))) (f' b)
valuesByIndex p f = let ixs = index p f
                        vals = values f
                    in fmap (\ks -> intersectionByKey ks vals) ixs

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
merge ps wi = ifoldl (\i b a -> b & at i .~ a) wi ps

merge' :: (At a, FoldableWithIndex (Index a) f) => (b -> Maybe (IxValue a) -> Maybe (IxValue a)) -> f b -> a -> a
merge' aux ps wi = ifoldl (\i b a -> b & at i %~ aux a) wi ps

differenceByKey :: (Foldable t, At a) => t (Index a) -> a -> a
differenceByKey ks wi = foldl (\b a -> b & at a .~ Nothing) wi ks

intersectionByKey :: (Ord (Index (f b)), At (f b), FoldableWithIndex (Index (f b)) f)
                 => Set (Index (f b))
                 -> f b
                 -> f b
intersectionByKey ks wi = differenceByKey (Set.difference (Set.fromList $ map fst $ itoList wi) ks) wi

nonEmptySet :: Set a -> Maybe (Set a)
nonEmptySet s = if Set.null s then Nothing else Just s

deleteFromIndex :: (Ord k, Ord a) => k -> a -> AppendMap k (Set a) -> AppendMap k (Set a)
deleteFromIndex k a m = Map.alter (nonEmptySet . Set.delete a . fromMaybe Set.empty) k m

addToIndex :: (Ord k, Ord a) => k -> a -> AppendMap k (Set a) -> AppendMap k (Set a)
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

instance HasValues (AppendMap k) (AppendMap k)
