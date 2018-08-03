{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Focus.App where

import Data.Aeson
import Data.Align
import Data.Proxy
import Data.Semigroup

import Reflex.Query.Class

import Focus.Account
import Focus.AppendMap (AppendMap)
import qualified Focus.AppendMap as AppendMap
import Focus.Request
import Focus.Sign

instance (Ord k, Query v) => Query (AppendMap k v) where
  type QueryResult (AppendMap k v) = AppendMap k (QueryResult v)
  crop q r = AppendMap.intersectionWith (flip crop) r q

singletonQuery :: (Monoid (QueryResult q), Ord k) => k -> QueryMorphism q (AppendMap k q)
singletonQuery k = QueryMorphism { _queryMorphism_mapQuery = AppendMap.singleton k
                                 , _queryMorphism_mapQueryResult = AppendMap.findWithDefault mempty k
                                 }

class ( ToJSON (ViewSelector app ()), FromJSON (ViewSelector app ())
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

newtype Version = Version { unVersion :: Int }

class IsVersion v where
  getVersion :: Proxy v -> Version
  getVersionNum :: Proxy v -> Int
  getVersionNum = unVersion . getVersion

class ( IsVersion v
      , Request (PublicRequest v app f)
      , Request (PrivateRequest v app f)
      , ToJSON (AppCredential app f)
      , FromJSON (AppCredential app f)
      ) => HasRequest v app (f :: * -> *) where
  data PublicRequest v app f :: * -> *
  data PrivateRequest v app f :: * -> *
  type AppCredential app f :: *
  type AppCredential app f = Signed (AuthToken f)
