{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, UndecidableInstances, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Focus.Backend.Schema.TH where

import Focus.Schema

import Data.Int
import Data.Proxy
import Database.Groundhog.Core
import Language.Haskell.TH
import Debug.Trace.LocationTH

class HasId a => DefaultKeyId a where
  toIdData :: Proxy a -> DefaultKey a -> IdData a
  fromIdData :: Proxy a -> IdData a -> DefaultKey a

class (IsUniqueKey (Key a (Unique (DefaultKeyUnique a))), IsUniqueKey (DefaultKey a)) => DefaultKeyIsUnique a where
  type DefaultKeyUnique a :: (* -> *) -> *
  defaultKeyToKey :: DefaultKey a -> Key a (Unique (DefaultKeyUnique a))

class GetByDefault a where
  getByDefault :: PersistBackend m => DefaultKey a -> m (Maybe a)

type IdDataIs a b = IdData a ~ b

class IsSumType a ~ HFalse => HasSingleConstructor a where
  type SingleConstructor a :: (* -> *) -> *
  singleConstructor :: Proxy a -> SingleConstructor a (ConstructorMarker a)

makeDefaultKeyIdInt64 :: Name -> Name -> Q [Dec]
makeDefaultKeyIdInt64 n k = do
  pv <- newName "pv"
  [d|
    instance IdDataIs $(conT n) Int64 => DefaultKeyId $(conT n) where
      toIdData _ dk = case $(lamE [conP k [varP pv]] (varE pv)) dk of
        PersistInt64 x -> x
        _ -> $failure "makeDefaultKeyIdInt64: pattern match failure (this should be impossible)"
      fromIdData _ = $(conE k) . PersistInt64
    |]

makeDefaultKeyIdSimple :: Name -> Name -> Q [Dec]
makeDefaultKeyIdSimple n k = do
  pv <- newName "pv"
  [d|
    instance DefaultKeyId $(conT n) where
      toIdData _ = $(lamE [conP k [varP pv]] (varE pv))
      fromIdData _ = $(conE k)
    |]

toId :: forall a. DefaultKeyId a => DefaultKey a -> Id a
toId = Id . toIdData (Proxy :: Proxy a)

fromId :: forall a. DefaultKeyId a => Id a -> DefaultKey a
fromId = fromIdData (Proxy :: Proxy a) . unId

deriving instance NeverNull (IdData a) => NeverNull (Id a)

instance (PersistField (DefaultKey a), DefaultKeyId a) => PersistField (Id a) where
  persistName = persistName 
  toPersistValues = toPersistValues . fromId
  fromPersistValues vs = do
    (a, vs') <- fromPersistValues vs
    return (toId a, vs')
  dbType p _ = dbType p (undefined :: DefaultKey a)

instance (PrimitivePersistField (DefaultKey a), DefaultKeyId a) => PrimitivePersistField (Id a) where
  toPrimitivePersistValue p = toPrimitivePersistValue p . fromId
  fromPrimitivePersistValue p = toId . fromPrimitivePersistValue p
