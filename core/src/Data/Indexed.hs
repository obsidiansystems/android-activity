{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Data.Indexed where

import Control.Lens.At
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Writer
import Data.Maybe

data Indexed k m = Indexed { _withIndex_project :: IxValue m -> k
                               , _withIndex_index :: Map k (Set (Index m))
                               , _withIndex_data :: m
                               }

type instance Index (Indexed k m) = Index m
type instance IxValue (Indexed k m) = IxValue m

nonEmptySet :: Set a -> Maybe (Set a)
nonEmptySet s = if Set.null s then Nothing else Just s

deleteFromIndex :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
deleteFromIndex k a m = Map.alter (nonEmptySet . Set.delete a . fromMaybe Set.empty) k m

addToIndex :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
addToIndex k a m = Map.alter (Just . Set.insert a . fromMaybe Set.empty) k m

instance (Ord (Index m), Ixed m, Ord k) => Ixed (Indexed k m) where
  ix = wiTransduce ix (:[])

instance (Ord (Index m), At m, Ord k) => At (Indexed k m) where
  at = wiTransduce at maybeToList

wiTransduce :: (Functor f, Ord k, Ord (Index m))
            => (Index m -> (a -> WriterT ([IxValue m], [IxValue m]) f a) -> m -> WriterT ([IxValue m], [IxValue m]) f m)
            -> (a -> [IxValue m])
            -> Index m
            -> (a -> f a)
            -> Indexed k m
            -> f (Indexed k m)
wiTransduce op toList a f (Indexed project index m) = (\(m', (olds, news)) -> Indexed project (foldl (\i new -> addToIndex (project new) a i) (foldl (\i old -> deleteFromIndex (project old) a i) index olds) news) m') <$> runWriterT (op a (\mold -> WriterT $ (\mnew -> (mnew, (toList mold, toList mnew))) <$> f mold) m)

-- test = Indexed length Map.empty Map.empty & at 5 .~ Just ("Hello, world!" :: String)
--                                             & at 6 .~ Just "asdfasdfasdfa"
