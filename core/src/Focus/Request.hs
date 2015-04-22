{-# LANGUAGE GADTs, QuasiQuotes, TemplateHaskell, ViewPatterns, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
module Focus.Request where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Parser
import qualified Data.Attoparsec.Lazy as LA
import qualified Data.ByteString.Lazy as LBS
import Language.Haskell.TH
import Control.Monad
import Control.Applicative
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.ByteString.Base64 as B64

data SomeRequest t where
    SomeRequest :: (FromJSON x, ToJSON x) => t x -> SomeRequest t

class Request r where
    requestToJSON :: r a -> Value
    requestParseJSON :: Value -> Parser (SomeRequest r)

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

data HCons a b = HCons a b
data HNil = HNil

instance (FromJSON a, FromJSON b) => FromJSON (HCons a b) where
  parseJSON (Array v) = do
    Just (aVal, v') <- return $ vectorView v
    a <- parseJSON aVal
    b <- parseJSON $ Array v'
    return $ HCons a b

instance FromJSON HNil where
  parseJSON (Array v) = do
    Nothing <- return $ vectorView v
    return HNil

class HListToJSON a where
  hListToJSON :: a -> [Value]

instance (ToJSON h, HListToJSON t) => HListToJSON (HCons h t) where
  hListToJSON (HCons h t) = toJSON h : hListToJSON t

instance HListToJSON HNil where
  hListToJSON HNil = []

instance HListToJSON (HCons a b) => ToJSON (HCons a b) where
  toJSON = Array . V.fromList . hListToJSON

instance ToJSON HNil where
  toJSON HNil = Array V.empty

makeRequest :: Name -> DecsQ
makeRequest n = do
  x <- reify n
  let cons = case x of
       (TyConI (DataD _ _ _ cs _)) -> cs
       (TyConI (NewtypeD _ _ _ c _)) -> [c]
  let wild = match wildP (normalB [|fail "invalid message"|]) []
  [d|
    instance Request $(conT n) where
      requestToJSON r = $(caseE [|r|] $ map conToJson cons)
      requestParseJSON v = do
        (tag, v') <- parseJSON v
        $(caseE [|tag :: String|] $ map (conParseJson (\body -> [|SomeRequest <$> $body|]) [|v'|]) cons ++ [wild])
    |]

makeJson :: Name -> DecsQ
makeJson n = do
  x <- reify n
  let cons = case x of
       (TyConI (DataD _ _ _ cs _)) -> cs
       (TyConI (NewtypeD _ _ _ c _)) -> [c]
  let wild = match wildP (normalB [|fail "invalid message"|]) []
  [d|
    instance ToJSON $(conT n) where
      toJSON r = $(caseE [|r|] $ map conToJson cons)
    instance FromJSON $(conT n) where
      parseJSON v = do
        (tag, v') <- parseJSON v
        $(caseE [|tag :: String|] $ map (conParseJson id [|v'|]) cons ++ [wild])
    |]

conParseJson :: (ExpQ -> ExpQ) -> ExpQ -> Con -> MatchQ
conParseJson wrapBody e c = do
  let name = conName c
  varNames <- replicateM (conArity c) $ newName "f"
  let fields = map varE varNames
      f c' = case c' of
        NormalC _ ts -> ts
        RecC _ ts -> map (\(_, s, t) -> (s, t)) ts
        InfixC t1 _ t2 -> [t1, t2]
        ForallC _ _ c'' -> f c''
  let ts = f c
  let tuple = foldr (\a b -> conP 'HCons [varP a, b]) (conP 'HNil []) varNames
      body = doE [ bindS tuple [|parseJSON $e|]
                 , noBindS [|return $(appsE (conE name : fields))|]
                 ]
  match (litP (StringL (nameBase name))) (normalB (wrapBody body)) []

conToJson :: Con -> MatchQ
conToJson c = do
  let name = conName c
      base = nameBase name
  varNames <- replicateM (conArity c) $ newName "f"
  let fields = map varE varNames
      f c' = case c' of
        NormalC _ ts -> ts
        RecC _ ts -> map (\(_, s, t) -> (s, t)) ts
        InfixC t1 _ t2 -> [t1, t2]
        ForallC _ _ c'' -> f c''
  let ts = f c
  let tuple = foldr (\a b -> appsE [conE 'HCons, varE a, b]) (conE 'HNil) varNames
      body = [|toJSON (base :: String, toJSON $tuple)|]
  match (conP name $ map varP varNames) (normalB body) []

conName :: Con -> Name
conName c = case c of
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ c' -> conName c'

conArity :: Con -> Int
conArity c = case c of
  NormalC _ ts -> length ts
  RecC _ ts -> length ts
  InfixC _ _ _ -> 2
  ForallC _ _ c' -> conArity c'

instance ToJSON ByteString where
    toJSON = toJSON . B64.encode
 
instance FromJSON ByteString64 where
    parseJSON o = either fail return . B64.decode =<< parseJSON o
