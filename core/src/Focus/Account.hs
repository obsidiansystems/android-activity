{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable, DefaultSignatures, FlexibleContexts, StandaloneDeriving, UndecidableInstances, DeriveGeneric #-}
module Focus.Account where

import Focus.Schema
import Focus.Request
import Data.Time
import Data.ByteString (ByteString)
import Data.Typeable
import Data.Aeson
import Focus.Sign
import GHC.Generics

data Account
  = Account { account_email :: Email
            , account_passwordHash :: Maybe ByteString
            , account_passwordResetNonce :: Maybe UTCTime
            }
  deriving (Show, Read, Eq, Ord, Typeable)

instance HasId Account

newtype PasswordResetToken = PasswordResetToken { unPasswordResetToken :: (Id Account, UTCTime) } deriving (Show, Read, Eq, Ord, ToJSON, FromJSON, Typeable)

newtype AuthToken = AuthToken { unAuthToken :: Id Account } deriving (Show, Read, Eq, Ord, ToJSON, FromJSON, Typeable)

data AccountRoute = AccountRoute_PasswordReset (Signed PasswordResetToken) deriving (Show, Read, Eq, Ord)

makeJson ''AccountRoute

data LoginError
  = LoginError_UserNotFound
  | LoginError_InvalidPassword
  deriving (Eq, Ord, Read, Generic)
instance FromJSON LoginError
instance ToJSON LoginError

instance Show LoginError where
  show LoginError_UserNotFound = "The user is not recognized"
  show LoginError_InvalidPassword = "Please enter a valid password"
