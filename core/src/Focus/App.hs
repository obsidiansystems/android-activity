{-# LANGUAGE TypeFamilies, FlexibleContexts, RankNTypes #-}
module Focus.App where

import Data.Aeson
import Data.Align
import Data.Semigroup

import Focus.AppendMap (AppendMap)
import qualified Focus.AppendMap as AppendMap
import Focus.Request

class (Monoid (QueryResult a), Semigroup (QueryResult a)) => Query a where
  type QueryResult a :: *
  crop :: a -> QueryResult a -> QueryResult a

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


class ( ToJSON (ViewSelector app ()), FromJSON (ViewSelector app ())
      , ToJSON (View app), FromJSON (View app)
      , Monoid (ViewSelector app ()), Semigroup (ViewSelector app ())
      , Query (ViewSelector app ()), QueryResult (ViewSelector app ()) ~ View app
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

