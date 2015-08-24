{-# LANGUAGE PolyKinds, GADTs, ScopedTypeVariables #-}
module Focus.Api where

import Data.Aeson
import Focus.Account
import Focus.Request
import Focus.Sign
import Data.HList

data ApiRequest :: (k -> *) -> (k -> *) -> k -> * where
  ApiRequest_Public :: public a -> ApiRequest public private a
  ApiRequest_Private :: Signed AuthToken -> private a -> ApiRequest public private a

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
      e -> error $ "Could not parse tag: " ++ e


