{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
#endif
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif

module Focus.Request where

import Control.Arrow
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Parser (value')
import Data.Align
import qualified Data.Attoparsec.Lazy as LA
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Map (DMap, DSum (..), GCompare (..))
import Data.List (isPrefixOf)
import Data.Maybe
import Data.Monoid hiding (First)
import Data.These
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.Haskell.TH
import Network.URI
import Reflex hiding (HList (..), Request)
import Data.Proxy

data SomeRequest t where
    SomeRequest :: (FromJSON x, ToJSON x) => t x -> SomeRequest t

class Request r where
  requestToJSON :: r a -> Value
  requestParseJSON :: Value -> Parser (SomeRequest r)
  requestResponseToJSON :: r a -> Dict (ToJSON a)
  requestResponseFromJSON :: r a -> Dict (FromJSON a)

instance Request r => FromJSON (SomeRequest r) where
  parseJSON v = requestParseJSON v

instance Request r => ToJSON (SomeRequest r) where
  toJSON (SomeRequest r) = requestToJSON r

decodeWith :: LA.Parser Value -> (Value -> Result a) -> LBS.ByteString -> Maybe a
decodeWith p to s =
  case LA.parse p s of
    LA.Done _ v -> case to v of
      Success a -> Just a
      _ -> Nothing
    _ -> Nothing

decodeValue' :: (FromJSON a) => LBS.ByteString -> Maybe a
decodeValue' = decodeWith value' fromJSON

vectorView :: Vector a -> Maybe (a, Vector a)
vectorView v =
  if V.length v > 0
  then Just (V.head v, V.tail v)
  else Nothing

data family HList (l::[*])

data instance HList '[] = HNil
data instance HList (x ': xs) = x `HCons` HList xs

infixr 2 `HCons`

deriving instance Eq (HList '[])
deriving instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs))

deriving instance Ord (HList '[])
deriving instance (Ord x, Ord (HList xs)) => Ord (HList (x ': xs))

deriving instance Bounded (HList '[])
deriving instance (Bounded x, Bounded (HList xs)) => Bounded (HList (x ': xs))

instance (FromJSON h, FromJSON (HList t)) => FromJSON (HList (h ': t)) where
  parseJSON = withArray "HList (a ': t)" $ \v -> do
    Just (aVal, v') <- return $ vectorView v
    a <- parseJSON aVal
    b <- parseJSON $ Array v'
    return $ HCons a b

instance FromJSON (HList '[]) where
  parseJSON = withArray "HList '[]" $ \v -> do
    Nothing <- return $ vectorView v
    return HNil

class HListToJSON l where
  hListToJSON :: HList l -> [Value]

instance (ToJSON h, HListToJSON t) => HListToJSON (h ': t) where
  hListToJSON (HCons h t) = toJSON h : hListToJSON t

instance HListToJSON '[] where
  hListToJSON HNil = []

instance HListToJSON l => ToJSON (HList l) where
  toJSON = Array . V.fromList . hListToJSON

decCons :: Dec -> [Con]
decCons = \case
#if MIN_VERSION_template_haskell(2,11,0)
  DataD _ _ _ _ cs _ -> cs
  NewtypeD _ _ _ _ c _ -> [c]
#else
  DataD _ _ _ cs _ -> cs
  NewtypeD _ _ _ c _ -> [c]
#endif
  _ -> error "undefined"

decTvbs :: Dec -> [TyVarBndr]
decTvbs = \case
#if MIN_VERSION_template_haskell(2,11,0)
  DataD _ _ tvbs _ _ _ -> tvbs
  NewtypeD _ _ tvbs _ _ _ -> tvbs
#else
  DataD _ _ tvbs _ _ -> tvbs
  NewtypeD _ _ tvbs _ _ -> tvbs
#endif
  _ -> error "undefined"

#ifdef USE_TEMPLATE_HASKELL
makeRequest :: Name -> DecsQ
makeRequest n = do
  x <- reify n
  let base = nameBase n
      toBeStripped = base <> "_"
      modifyConName cn = if length cons == 1 then cn else if toBeStripped `isPrefixOf` cn then drop (length toBeStripped) cn else error $ "makeRequest: expecting name beginning with " <> show toBeStripped <> ", got " <> show cn
      cons = case x of
       TyConI d -> decCons d
       _ -> error "undefined"
  let wild = match wildP (normalB [|fail "invalid message"|]) []
  [d|
    instance Request $(conT n) where
      requestToJSON r = $(caseE [|r|] $ map (conToJson modifyConName) cons)
      requestParseJSON v = do
        (tag', v') <- parseJSON v
        $(caseE [|tag' :: String|] $ map (conParseJson modifyConName (\body -> [|SomeRequest <$> $body|]) [|v'|]) cons ++ [wild])
      requestResponseToJSON r = $(caseE [|r|] $ map (\c -> match (conP (conName c) $ replicate (conArity c) wildP) (normalB [|Dict|]) []) cons)
      requestResponseFromJSON r = $(caseE [|r|] $ map (\c -> match (conP (conName c) $ replicate (conArity c) wildP) (normalB [|Dict|]) []) cons)
    |]

makeRequestForDataInstance :: Name -> Name -> DecsQ
makeRequestForDataInstance n n' = do
  x <- reify n
  let base = nameBase n
      toBeStripped = base <> "_"
      modifyConName cn = if length cons == 1 then cn else if toBeStripped `isPrefixOf` cn then drop (length toBeStripped) cn else error $ "makeRequest: expecting name beginning with " <> show toBeStripped <> ", got " <> show cn
      cons = case x of
                  (FamilyI _ dataInstances) -> do
#if MIN_VERSION_template_haskell(2,11,0)
                    (DataInstD _ _ (ConT m:_) _ xs _) <- dataInstances
#else
                    (DataInstD _ _ (ConT m:_) xs _) <- dataInstances
#endif
                    guard $ m == n'
                    xs
                  _ -> error "undefined"
  let wild = match wildP (normalB [|fail "invalid message"|]) []
  [d|
    instance Request $(appT (appT (conT n) (conT n')) (varT $ mkName "f")) where
      requestToJSON r = $(caseE [|r|] $ map (conToJson modifyConName) cons)
      requestParseJSON v = do
        (tag', v') <- parseJSON v
        $(caseE [|tag' :: String|] $ map (conParseJson modifyConName (\body -> [|SomeRequest <$> $body|]) [|v'|]) cons ++ [wild])
      requestResponseToJSON r = $(caseE [|r|] $ map (\c -> match (conP (conName c) $ replicate (conArity c) wildP) (normalB [|Dict|]) []) cons)
      requestResponseFromJSON r = $(caseE [|r|] $ map (\c -> match (conP (conName c) $ replicate (conArity c) wildP) (normalB [|Dict|]) []) cons)
    |]
#endif

-- | Extracts the name from a type variable binder.
tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name  ) = name
tvbName (KindedTV name _) = name

#ifdef USE_TEMPLATE_HASKELL
makeJson :: Name -> DecsQ
makeJson n = do
  x <- reify n
  let base = nameBase n
      toBeStripped = base <> "_"
      modifyConName cn = if length cons == 1 then cn else if toBeStripped `isPrefixOf` cn then drop (length toBeStripped) cn else error $ "makeRequest: expecting name beginning with " <> show toBeStripped <> ", got " <> show cn
      cons = case x of
       TyConI d -> decCons d
       _ -> error "undefined"
      typeNames = map tvbName $ case x of
       TyConI d -> decTvbs d
       _ -> error "undefined"
  let wild = match wildP (normalB [|fail "invalid message"|]) []
  [d|
    instance ToJSON $(foldl appT (conT n) $ map varT typeNames)   where
      toJSON r = $(caseE [|r|] $ map (conToJson modifyConName) cons)
    instance FromJSON $(foldl appT (conT n) $ map varT typeNames) where
      parseJSON v = do
        (tag', v') <- parseJSON v
        $(caseE [|tag' :: String|] $ map (conParseJson modifyConName id [|v'|]) cons ++ [wild])
    |]

makeJsonForDataInstance :: Name -> [Maybe Name] -> DecsQ
makeJsonForDataInstance n mns = do
  x <- reify n
  let base = nameBase n
      toBeStripped = base <> "_"
      matchingInstances {- :: [([Type], [Con])] -} = case x of
       (FamilyI _ dataInstances) -> do
#if MIN_VERSION_template_haskell(2,11,0)
         (DataInstD _ _ tParams _ xs _) <- dataInstances
#else
         (DataInstD _ _ tParams xs _) <- dataInstances
#endif

         typeNames <- maybeToList $ forM (align mns tParams) $ \case
           These Nothing v@(VarT _) -> Just v
           These (Just n') c@(ConT m) | m == n' -> Just c
           _ -> Nothing
         return (typeNames, xs)
       _ -> error "undefined"
      -- typeNames = case x of
      --  (FamilyI _ _) -> [conT n']
      --  _ -> error "undefined"
  (typeNames, cons) <- case matchingInstances of
    [matchingInstance] -> return matchingInstance
    [] -> fail $ "makeJsonForDataInstance: No Data Instances found for pattern " <> show (n, mns)
    _ -> fail $ "makeJsonForDataInstance: Ambiguous pattern " <> show (n, mns)

  let modifyConName cn = if length cons == 1 then cn else if toBeStripped `isPrefixOf` cn then drop (length toBeStripped) cn else error $ "makeRequest: expecting name beginning with " <> show toBeStripped <> ", got " <> show cn
  let wild = match wildP (normalB [|fail "invalid message"|]) []
  [d|
    instance ToJSON $(foldl appT (conT n) (map return typeNames))   where
      toJSON r = $(caseE [|r|] $ map (conToJson modifyConName) cons)
    instance FromJSON $(foldl appT (conT n) (map return typeNames)) where
      parseJSON v = do
        (tag', v') <- parseJSON v
        $(caseE [|tag' :: String|] $ map (conParseJson modifyConName id [|v'|]) cons ++ [wild])
    |]

conParseJson :: (String -> String) -> (ExpQ -> ExpQ) -> ExpQ -> Con -> MatchQ
conParseJson modifyName wrapBody e c = do
  let name = conName c
  varNames <- replicateM (conArity c) $ newName "f"
  let fields = map varE varNames
      tuple = foldr (\a b -> conP 'HCons [varP a, b]) (conP 'HNil []) varNames
      body = doE [ bindS tuple [|parseJSON $e|]
                 , noBindS [|return $(appsE (conE name : fields))|]
                 ]
  match (litP (StringL (modifyName $ nameBase name))) (normalB (wrapBody body)) []

conToJson :: (String -> String) -> Con -> MatchQ
conToJson modifyName c = do
  let name = conName c
      base = nameBase name
      tag' = modifyName base
  varNames <- replicateM (conArity c) $ newName "f"
  let tuple = foldr (\a b -> appsE [conE 'HCons, varE a, b]) (conE 'HNil) varNames
      body = [|toJSON (tag' :: String, toJSON $tuple)|]
  match (conP name $ map varP varNames) (normalB body) []
#endif

conName :: Con -> Name
conName c = case c of
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ c' -> conName c'
#if MIN_VERSION_template_haskell(2,11,0)
  GadtC [n] _ _ -> n
  RecGadtC [n] _ _ -> n
  _ -> error "conName: GADT constructors with multiple names not yet supported"
#endif

conArity :: Con -> Int
conArity c = case c of
  NormalC _ ts -> length ts
  RecC _ ts -> length ts
  InfixC _ _ _ -> 2
  ForallC _ _ c' -> conArity c'
#if MIN_VERSION_template_haskell(2,11,0)
  GadtC _ ts _ -> length ts
  RecGadtC _ ts _ -> length ts
#endif

class Functor' (f :: (k -> *) -> *) where
  fmap' :: (forall x. a x -> b x) -> f a -> f b

ffor' :: Functor' f => f a -> (forall x. a x -> b x) -> f b
ffor' x f = fmap' f x

class Foldable' f where
  foldr' :: (forall x. a x -> b -> b) -> b -> f a -> b

mapM_' :: (Foldable' f, Monad m) => (forall x. a x -> m b) -> f a -> m ()
mapM_' f = foldr' ((>>) . f) (return ())

forM_' :: (Foldable' f, Monad m) => f a -> (forall x. a x -> m b) -> m ()
forM_' xs f = mapM_' f xs

toListWith' :: Foldable' f => (forall x. a x -> b) -> f a -> [b]
toListWith' f t = foldr' ((:) . f) [] t

class (Functor' t, Foldable' t) => Traversable' (t :: (k -> *) -> *) where
  mapM' :: Monad m => (forall x. a x -> m (b x)) -> t a -> m (t b)

data With' a (f :: k -> *) (x :: k) = With' a (f x)

data Some (f :: k -> *) = forall a. Some (f a)

instance Functor' Some where
  fmap' f (Some x) = Some $ f x

instance Foldable' Some where
  foldr' f z (Some x) = f x z

instance Traversable' Some where
  mapM' f (Some x) = liftM Some $ f x

instance ToJSON' f => ToJSON (Some f) where
  toJSON (Some x) = toJSON' x

instance FromJSON' f => FromJSON (Some f) where
  parseJSON = parseJSON'

-- | Sequentially number the contents of the Traversable'
numberAndSize' :: forall t a. Traversable' t => t a -> (t (With' Int a), Int)
numberAndSize' t = (t', sizePlusOne - 1)
  where f :: forall x. a x -> State Int (With' Int a x)
        f a = do
          n <- get
          modify succ
          return $ With' n a
        (t', sizePlusOne) = runState (mapM' f t) 1

class ToJSON' f where
  toJSON' :: f a -> Value

class FromJSON' f where
  parseJSON' :: Value -> Parser (Some f)

fromJSON' :: FromJSON' f => Value -> Result (Some f)
fromJSON' = parse parseJSON'

#ifdef USE_TEMPLATE_HASKELL
makeJson' :: Name -> DecsQ
makeJson' n = do
  x <- reify n
  let cons = case x of
       TyConI d -> decCons d
       _ -> error "undefined"
      base = nameBase n
      toBeStripped = base <> "_"
      modifyConName cn = if length cons == 1 then cn else if toBeStripped `isPrefixOf` cn then drop (length toBeStripped) cn else error $ "makeRequest: expecting name beginning with " <> show toBeStripped <> ", got " <> show cn
  let wild = match wildP (normalB [|fail "invalid message"|]) []
  [d|
    instance ToJSON' $(conT n) where
      toJSON' r = $(caseE [|r|] $ map (conToJson modifyConName) cons)
    instance FromJSON' $(conT n) where
      parseJSON' v = do
        (tag', v') <- parseJSON v
        $(caseE [|tag' :: String|] $ map (conParseJson modifyConName (\body -> [|Some <$> $body|]) [|v'|]) cons ++ [wild])
    |]
#endif

class AllArgsHave c f where
  getArgDict :: f x -> Dict (c x)

instance AllArgsHave FromJSON f => AllArgsHave (ComposeConstraint FromJSON Identity) f where
  getArgDict (x :: f x) = case getArgDict x :: Dict (FromJSON x) of
    Dict -> Dict

instance (FromJSON l, AllArgsHave FromJSON f) => AllArgsHave (ComposeConstraint FromJSON (Either l)) f where
  getArgDict (x :: f x) = case getArgDict x :: Dict (FromJSON x) of
    Dict -> Dict

class c (f x) => ComposeConstraint c f x
instance c (f x) => ComposeConstraint c f x

data WithClass c a = c a => WithClass a

deriving instance Show (WithClass Show a)

deriving instance Show (Some (WithClass Show))

instance (FromJSON a, c a) => FromJSON (WithClass c a) where
  parseJSON = liftM WithClass . parseJSON

data Of x f = Of (f x)

instance Functor' (Of x) where
  fmap' f (Of a) = Of $ f a

instance Foldable' (Of x) where
  foldr' f z (Of a) = f a z

instance Traversable' (Of x) where
  mapM' f (Of a) = liftM Of $ f a

deriving instance Show (f x) => Show (Of x f)

data Of2 :: (k -> k -> (k -> *) -> *) where
  Of2a :: f a -> Of2 a b f
  Of2b :: f b -> Of2 a b f

instance Functor' (Of2 x y) where
  fmap' f (Of2a a) = Of2a $ f a
  fmap' f (Of2b b) = Of2b $ f b

instance Foldable' (Of2 x y) where
  foldr' f z (Of2a a) = f a z
  foldr' f z (Of2b b) = f b z

instance Traversable' (Of2 x y) where
  mapM' f (Of2a a) = liftM Of2a $ f a
  mapM' f (Of2b b) = liftM Of2b $ f b

newtype Map' (k :: a -> *) (v :: a -> *) = Map' { unMap' :: DMap k v }

instance Functor' (Map' k) where
  fmap' f = Map' . DMap.mapWithKey (\_ -> f) . unMap'

instance Foldable' (Map' k) where
  foldr' f z (Map' dm) = foldr (\(_ :=> v) a -> f v a) z $ DMap.toList dm

instance Traversable' (Map' k) where
  mapM' f (Map' dm) = liftM (Map' . DMap.fromDistinctAscList) $ mapM (\(k :=> v) -> f v >>= \v' -> return $ k :=> v') $ DMap.toList dm

map'Singleton :: k a -> v a -> Map' k v
map'Singleton k v = Map' $ DMap.singleton k v

map'Lookup :: GCompare k => k a -> Map' k v -> Maybe (v a)
map'Lookup k (Map' dm) = DMap.lookup k dm

type family HAppendListR (l1 :: [k]) (l2 :: [k]) :: [k]
type instance HAppendListR '[] l = l
type instance HAppendListR (e ': l) l' = e ': HAppendListR l l'

fhAppend :: FHList f l1 -> FHList f l2 -> FHList f (HAppendListR l1 l2)
fhAppend l1 l2 = case l1 of
  FHCons h t -> FHCons h $ fhAppend t l2
  FHNil -> l2

data HIndex :: [k] -> k -> * where
  Here :: HIndex (h ': t) h
  Next :: HIndex t x -> HIndex (h ': t) x

class HIsPrefixOf l1 l2 where
  lengthenHIndex :: HIndex l1 a -> HIndex l2 a

instance HIsPrefixOf t1 t2 => HIsPrefixOf (h ': t1) (h ': t2) where
  lengthenHIndex i = case i of
    Here -> Here
    Next x -> Next $ lengthenHIndex x

type BlahInternal t f g m a l = EventSelector t (HIndex l) -> m (a, FHList (Event t) l)

data Blah t f g m a
   = forall (l :: [*]). IncreaseHIndex l => Blah (BlahInternal t f g m a l)

instance Functor m => Functor (Blah t f g m) where
  fmap f (Blah b) = Blah $ \es -> fmap (first f) $ b es

expandHIndex :: forall l1 l2 a. Proxy l2 -> HIndex l1 a -> HIndex (HAppendListR l1 l2) a
expandHIndex _ i = case i of
  Here -> Here
  Next x -> Next $ expandHIndex (Proxy :: Proxy l2) x

class IncreaseHIndex l1 where
  increaseHIndex :: Proxy l1 -> HIndex l2 a -> HIndex (HAppendListR l1 l2) a
  appendIncreaseHIndexInstance :: IncreaseHIndex l2 => Proxy l1 -> Proxy l2 -> Dict (IncreaseHIndex (HAppendListR l1 l2))

instance IncreaseHIndex '[] where
  increaseHIndex _ i = i
  appendIncreaseHIndexInstance _ _ = Dict

instance IncreaseHIndex t => IncreaseHIndex (h ': t) where
  increaseHIndex _ i = Next $ increaseHIndex (Proxy :: Proxy t) i
  appendIncreaseHIndexInstance _ l2 = case appendIncreaseHIndexInstance (Proxy :: Proxy t) l2 of
    Dict -> Dict

instance Applicative m => Applicative (Blah t f g m) where
  pure a = Blah $ \(_ :: EventSelector t (HIndex '[])) -> pure (a, FHNil)
  Blah (f :: BlahInternal t f g m (a -> b) l1) <*> Blah (x :: BlahInternal t f g m a l2) = case appendIncreaseHIndexInstance (Proxy :: Proxy l1) (Proxy :: Proxy l2) of
    Dict -> Blah $ \(es :: EventSelector t (HIndex (HAppendListR l1 l2))) ->
      let combine (fResult, fHList) (xResult, xHList) = (fResult xResult, fHList `fhAppend` xHList)
          es1 :: EventSelector t (HIndex l1)
          es1 = EventSelector $ \k -> select es $ expandHIndex (Proxy :: Proxy l2) k
          es2 :: EventSelector t (HIndex l2)
          es2 = EventSelector $ \k -> select es $ increaseHIndex (Proxy :: Proxy l1) k
      in combine <$> f es1 <*> x es2

sendApi :: Monad m => Event t (f a) -> Blah t f g m (Event t (g a))
sendApi = undefined

encodeURIComponent :: String -> String
encodeURIComponent = escapeURIString (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_.!~*'()")
