{-# LANGUAGE PolyKinds, GADTs, ScopedTypeVariables, TemplateHaskell, LambdaCase #-}
module Focus.Api where

import Data.Aeson
import Data.Constraint
import Focus.App
import Focus.Request

import Debug.Trace.LocationTH

data ApiRequest (f :: * -> *) :: * -> ((* -> *) -> k -> *) -> ((* -> *) -> k -> *) -> k -> * where
  ApiRequest_Public :: public f a -> ApiRequest f cred public private a
  ApiRequest_Private :: cred -> private f a -> ApiRequest f cred public private a
  deriving (Show)

type AppRequest f app = ApiRequest f (AppCredential app f) (PublicRequest app) (PrivateRequest app)

instance (Request (private f), Request (public f), ToJSON cred, FromJSON cred) => Request (ApiRequest f cred public private) where
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
      e -> $failure $ "Could not parse tag: " ++ e
  requestResponseToJSON = \case
    ApiRequest_Public p -> requestResponseToJSON p
    ApiRequest_Private _ p -> requestResponseToJSON p
  requestResponseFromJSON = \case
    ApiRequest_Public p -> requestResponseFromJSON p
    ApiRequest_Private _ p -> requestResponseFromJSON p

public :: PublicRequest app f t -> AppRequest f app t
public = ApiRequest_Public

private :: AppCredential app f -> PrivateRequest app f t -> AppRequest f app t
private = ApiRequest_Private
