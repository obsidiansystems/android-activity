{-# LANGUAGE PolyKinds, GADTs, ScopedTypeVariables, TemplateHaskell, LambdaCase #-}
module Focus.Api where

import Data.Aeson
import Data.Constraint
import Focus.Account
import Focus.App
import Focus.Request
import Focus.Sign

import Debug.Trace.LocationTH

data ApiRequest :: (k -> *) -> (k -> *) -> k -> * where
  ApiRequest_Public :: public a -> ApiRequest public private a
  ApiRequest_Private :: Signed AuthToken -> private a -> ApiRequest public private a
  deriving (Show)

type AppRequest app = ApiRequest (PublicRequest app) (PrivateRequest app)

instance (Request private, Request public) => Request (ApiRequest public private) where
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

public :: PublicRequest app t -> AppRequest app t
public = ApiRequest_Public

private :: Signed AuthToken -> PrivateRequest app t -> AppRequest app t
private = ApiRequest_Private

