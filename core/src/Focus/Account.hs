{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable, DefaultSignatures, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module Focus.Account where

import Focus.Schema
import Focus.Request
import Data.Time
import Data.ByteString (ByteString)
import Data.Typeable
import Data.Aeson
import Focus.Sign

data Account
  = Account { account_email :: Email
            , account_passwordHash :: Maybe ByteString
            , account_passwordResetNonce :: Maybe UTCTime
            }
  deriving (Show, Read, Eq, Ord, Typeable)

instance HasId Account where
  type IdData Account = Email -- Accounts are identified by their email addresses

newtype PasswordResetToken = PasswordResetToken { unPasswordResetToken :: (Id Account, UTCTime) } deriving (Show, Read, Eq, Ord, ToJSON, FromJSON, Typeable)

newtype AuthToken = AuthToken { unAuthToken :: Id Account } deriving (Show, Read, Eq, Ord, ToJSON, FromJSON, Typeable)

data AccountRoute = AccountRoute_PasswordReset (Signed PasswordResetToken) deriving (Show, Read, Eq, Ord)

makeJson ''AccountRoute
