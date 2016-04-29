{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, TemplateHaskell, DeriveGeneric, DeriveFunctor, DeriveTraversable #-}
module Focus.AppendMap where

import Control.Lens
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Typeable
import GHC.Generics (Generic)

import Data.Indexed

newtype AppendMap k m = AppendMap { _unAppendMap :: Map k m }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, Functor, Foldable, Traversable)

type instance (Index (AppendMap k m)) = k
type instance (IxValue (AppendMap k m)) = m

instance Ord k => Ixed (AppendMap k m) where
  ix i = _Wrapped . ix i

instance Ord k => At (AppendMap k m) where
  at i = _Wrapped . at i

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
    fmap AppendMap . sequence . Map.fromListWithKey (fail "duplicate key in JSON deserialization of AppendMap") . map (fmap return) $ res

instance HasValues (AppendMap k) (AppendMap k)

makeWrapped ''AppendMap
