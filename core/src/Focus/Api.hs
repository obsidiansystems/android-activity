{-# LANGUAGE PolyKinds, GADTs, ScopedTypeVariables, TemplateHaskell #-}
module Focus.Api where

import Data.Aeson
import Focus.Account
import Focus.App
import Focus.Request
import Focus.Sign
import Data.HList

import Debug.Trace.LocationTH

data ApiRequest :: (k -> *) -> (k -> *) -> k -> * where
  ApiRequest_Public :: public a -> ApiRequest public private a
  ApiRequest_Private :: Signed AuthToken -> private a -> ApiRequest public private a
  deriving (Show)

instance (Request private, Request public) => Request (ApiRequest public private) where
  requestToJSON r = case r of
    ApiRequest_Public p -> toJSON ("Public"::String, SomeRequest p `HCons` HNil)
    ApiRequest_Private token p -> toJSON ("Private"::String, token `HCons` SomeRequest p `HCons` HNil)
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

public :: PublicRequest app t -> ApiRequest (PublicRequest app) (PrivateRequest app) t
public = ApiRequest_Public

private :: Signed AuthToken -> PrivateRequest app t -> ApiRequest (PublicRequest app) (PrivateRequest app) t
private = ApiRequest_Private

