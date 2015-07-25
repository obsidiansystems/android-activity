{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, FlexibleContexts, NoMonomorphismRestriction #-}
module Focus.Backend.Account where

import Focus.Backend.DB
import Focus.Backend.Email
import Focus.Backend.Schema.TH

import Focus.Account
import Focus.Brand
import Focus.Misc
import Focus.Sign
import Focus.Route
import Focus.Schema

import Control.Monad.Writer
import Crypto.PasswordStore
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import Database.Groundhog
import Database.Groundhog.TH
import qualified Data.Map as Map
import qualified Data.Text as T

mkPersist defaultCodegenConfig [groundhog|
  - entity: Account
    autoKey: null
    keys:
      - name: AccountId
        default: true
    constructors:
      - name: Account
        uniques:
        - name: AccountId
          type: primary
          fields: [account_email]
|]

makeDefaultKeyIdSimple ''Account 'AccountIdKey

migrateAccount = migrate (undefined :: Account)

-- Returns whether a new account had to be created
ensureAccountExists :: (PersistBackend m, MonadSign m, MonadRoute m, MonadEmail m, MonadBrand m) => Id Account -> m Bool
ensureAccountExists aid = do
  nonce <- getTime
  result <- insertBy AccountId $ Account (unId aid) Nothing (Just nonce)
  return $ isRight result

generatePasswordResetToken aid = do
  nonce <- getTime
  sign $ PasswordResetToken (aid, nonce)

setAccountPassword :: (PersistBackend m, MonadIO m) => Id Account -> Text -> m ()
setAccountPassword aid password = do
  salt <- liftIO genSaltIO
  update [Account_passwordHashField =. Just (makePasswordSaltWith pbkdf2 (2^) (encodeUtf8 password) salt 14), Account_passwordResetNonceField =. (Nothing :: Maybe UTCTime)] (Account_emailField ==. unId aid)

setPasswordWithToken :: (MonadIO m, PersistBackend m, MonadSign m) => Signed AuthToken -> Text -> m ()
setPasswordWithToken token password = do
  Just (AuthToken aid) <- readSigned token
  setAccountPassword aid password

resetPasswordWithToken :: (MonadIO m, PersistBackend m, MonadSign m) => Signed PasswordResetToken -> Text -> m (Id Account)
resetPasswordWithToken prt password = do
  Just (PasswordResetToken (aid, nonce)) <- readSigned prt
  Just a <- getBy $ fromId aid
  True <- return $ account_passwordResetNonce a == Just nonce
  setAccountPassword aid password
  return aid

