{-# LANGUAGE CPP, OverloadedStrings, GADTs, ScopedTypeVariables, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable, DefaultSignatures, FlexibleContexts, StandaloneDeriving, UndecidableInstances, DeriveGeneric #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
#endif
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

newtype PasswordResetToken f = PasswordResetToken { unPasswordResetToken :: (f (Id Account), UTCTime) }

newtype AuthToken f = AuthToken { unAuthToken :: f (Id Account) } deriving (Typeable)

deriving instance (Show (f (Id Account))) => Show (AuthToken f)
deriving instance (Read (f (Id Account))) => Read (AuthToken f)
deriving instance (Eq (f (Id Account))) => Eq (AuthToken f)
deriving instance (Ord (f (Id Account))) => Ord (AuthToken f)
deriving instance (ToJSON (f (Id Account))) => ToJSON (AuthToken f)
deriving instance (FromJSON (f (Id Account))) => FromJSON (AuthToken f)

deriving instance (Show (f (Id Account))) => Show (PasswordResetToken f)
deriving instance (Read (f (Id Account))) => Read (PasswordResetToken f)
deriving instance (Eq (f (Id Account))) => Eq (PasswordResetToken f)
deriving instance (Ord (f (Id Account))) => Ord (PasswordResetToken f)
deriving instance (ToJSON (f (Id Account))) => ToJSON (PasswordResetToken f)
deriving instance (FromJSON (f (Id Account))) => FromJSON (PasswordResetToken f)

data AccountRoute f = AccountRoute_PasswordReset (Signed (PasswordResetToken f)) deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
makeJson ''AccountRoute
#else
instance ToJSON (AccountRoute f_a2ovT) where
  toJSON r_a2Ctk
    = case r_a2Ctk of {
        AccountRoute_PasswordReset f_a2Ctn
          -> toJSON
               ("AccountRoute_PasswordReset" :: String,
              toJSON (HCons f_a2Ctn HNil)) }
instance FromJSON (AccountRoute f_a2ovT) where
  parseJSON v_a2Ctt
    = do { (tag'_a2Ctu, v'_a2Ctw) <- parseJSON v_a2Ctt;
           case tag'_a2Ctu :: String of
             "AccountRoute_PasswordReset"
               -> do { HCons f_a2Ctz HNil <- parseJSON v'_a2Ctw;
                       return (AccountRoute_PasswordReset f_a2Ctz) }
             _ -> fail "invalid message" }
#endif

data LoginError
  = LoginError_UserNotFound
  | LoginError_InvalidPassword
  deriving (Eq, Ord, Read, Generic)
instance FromJSON LoginError
instance ToJSON LoginError

instance Show LoginError where
  show LoginError_UserNotFound = "The user is not recognized"
  show LoginError_InvalidPassword = "Please enter a valid password"
