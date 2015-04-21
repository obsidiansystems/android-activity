{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable, DefaultSignatures, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module Focus.Account where

import Focus.Schema
import Focus.Request
import Data.Text
import Data.Time
import Data.ByteString (ByteString)
import Data.Typeable
import Data.Aeson

data Account
  = Account { account_email :: Email
            , account_passwordHash :: Maybe ByteString
            , account_passwordResetNonce :: Maybe UTCTime
            }
  deriving (Show, Read, Eq, Ord, Typeable)

instance HasId Account where
  type IdData Account = Text -- Accounts are identified by their email addresses

newtype Signed a = Signed { unSigned :: Text } deriving (Show, Read, Eq, Ord, ToJSON, FromJSON)

class Monad m => MonadSign m where
  sign :: (Typeable a, ToJSON a) => a -> m (Signed a) -- We need the Typeable here because otherwise two Signeds whose contents encode the same way will be interchangeable
  readSigned :: (Typeable a, FromJSON a) => Signed a -> m (Maybe a)

newtype PasswordResetToken = PasswordResetToken { unPasswordResetToken :: (Id Account, UTCTime) } deriving (Show, Read, Eq, Ord, ToJSON, FromJSON, Typeable)

newtype AuthToken = AuthToken { unAuthToken :: Id Account } deriving (Show, Read, Eq, Ord, ToJSON, FromJSON, Typeable)


