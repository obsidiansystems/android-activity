{-# LANGUAGE TypeFamilies, FlexibleContexts, RankNTypes, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Focus.App where

import Data.Aeson
import Data.Align
import Data.Bits
import Data.Data
import Data.Ix
import Data.Semigroup
import Foreign.Storable

import Reflex.Patch

import Focus.AppendMap (AppendMap)
import qualified Focus.AppendMap as AppendMap
import Focus.Request

class (Monoid (QueryResult a), Semigroup (QueryResult a)) => Query a where
  type QueryResult a :: *
  crop :: a -> QueryResult a -> QueryResult a

-- | NB: QueryMorphism's must be group homomorphisms when acting on the query type
-- and compatible with the query relationship when acting on the query result
data QueryMorphism q q' = QueryMorphism
  { _queryMorphism_mapQuery :: q -> q'
  , _queryMorphism_mapQueryResult :: QueryResult q' -> QueryResult q
  }

instance (Ord k, Query v) => Query (AppendMap k v) where
  type QueryResult (AppendMap k v) = AppendMap k (QueryResult v)
  crop q r = AppendMap.intersectionWith (flip crop) r q

singletonQuery :: (Monoid (QueryResult q), Ord k) => k -> QueryMorphism q (AppendMap k q)
singletonQuery k = QueryMorphism { _queryMorphism_mapQuery = AppendMap.singleton k
                                 , _queryMorphism_mapQueryResult = AppendMap.findWithDefault mempty k
                                 }

-- | This type keeps track of the multiplicity of elements of the view selector that are being used by the app
newtype SelectedCount = SelectedCount { unSelectedCount :: Int }
  deriving (Eq, Ord, Show, Read, Integral, Num, Bounded, Enum, Real, Ix, Bits, FiniteBits, Storable, Data, ToJSON, FromJSON)

instance Semigroup SelectedCount where
  SelectedCount a <> SelectedCount b = SelectedCount (a + b)

instance Monoid SelectedCount where
  mempty = SelectedCount 0
  mappend = (<>)

instance Group SelectedCount where
  negateG (SelectedCount a) = SelectedCount (negate a)

instance Additive SelectedCount

-- | The Semigroup/Monoid/Group instances for a ViewSelector should use this function which returns Nothing if the result is 0. This allows the pruning of leaves that are no longer wanted.
combineSelectedCounts :: SelectedCount -> SelectedCount -> Maybe SelectedCount
combineSelectedCounts (SelectedCount i) (SelectedCount j) = case i == j of
  True -> Nothing
  False -> Just $ SelectedCount (i + j)

class ( ToJSON (ViewSelector app SelectedCount), FromJSON (ViewSelector app SelectedCount)
      , ToJSON (View app), FromJSON (View app)
      , Monoid (ViewSelector app SelectedCount), Semigroup (ViewSelector app SelectedCount)
      , Query (ViewSelector app SelectedCount), QueryResult (ViewSelector app SelectedCount) ~ View app
      , Align (ViewSelector app)
      , Eq (View app)
      , Show (View app)
      ) => HasView app where
  type View app
  type ViewSelector app :: * -> *

cropView :: (Query q) => q -> QueryResult q -> QueryResult q
cropView = crop

class (Request (PublicRequest app), Request (PrivateRequest app)) => HasRequest app where
  data PublicRequest app :: * -> *
  data PrivateRequest app :: * -> *

