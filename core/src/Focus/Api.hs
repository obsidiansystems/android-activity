{-# LANGUAGE CPP, PolyKinds, GADTs, ScopedTypeVariables, LambdaCase #-}
module Focus.Api where

import Data.Aeson
import Data.Constraint
import Focus.Account
import Focus.App
import Focus.Request
import Focus.Sign

data ApiRequest (f :: * -> *) :: ((* -> *) -> k -> *) -> ((* -> *) -> k -> *) -> k -> * where
  ApiRequest_Public :: public f a -> ApiRequest f public private a
  ApiRequest_Private :: Signed (AuthToken f) -> private f a -> ApiRequest f public private a
  deriving (Show)

type AppRequest f app = ApiRequest f (PublicRequest app) (PrivateRequest app)

instance (Request (private f), Request (public f)) => Request (ApiRequest f public private) where
  requestToJSON r = case r of
    ApiRequest_Public p -> case (requestResponseToJSON p, requestResponseFromJSON p) of
      (Dict, Dict) -> toJSON ("Public"::String, SomeRequest p `HCons` HNil)
    ApiRequest_Private token p -> case (requestResponseToJSON p, requestResponseFromJSON p) of
      (Dict, Dict) -> toJSON ("Private"::String, token `HCons` SomeRequest p `HCons` HNil)
  requestParseJSON v = do
    (tag, body) <- parseJSON v
    case tag of
      ("Public"::String) -> do
        SomeRequest p `HCons` HNil <- parseJSON body
        return $ SomeRequest $ ApiRequest_Public p
      ("Private"::String) -> do
        token `HCons` SomeRequest p `HCons` HNil <- parseJSON body
        return $ SomeRequest $ ApiRequest_Private token p
      e -> error $ "Could not parse tag: " ++ e
  requestResponseToJSON = \case
    ApiRequest_Public p -> requestResponseToJSON p
    ApiRequest_Private _ p -> requestResponseToJSON p
  requestResponseFromJSON = \case
    ApiRequest_Public p -> requestResponseFromJSON p
    ApiRequest_Private _ p -> requestResponseFromJSON p

public :: PublicRequest app f t -> AppRequest f app t
public = ApiRequest_Public

private :: Signed (AuthToken f) -> PrivateRequest app f t -> AppRequest f app t
private = ApiRequest_Private
