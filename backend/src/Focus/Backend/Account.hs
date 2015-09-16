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

import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Crypto.PasswordStore
import Data.Default
import Data.String
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.TH
import qualified Data.Text as T
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Data.List.NonEmpty

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

migrateAccount :: PersistBackend m => Migration m
migrateAccount = migrate (undefined :: Account)

-- Returns whether a new account had to be created
ensureAccountExists :: (PersistBackend m, MonadSign m) => Id Account -> m Bool
ensureAccountExists aid = do
  nonce <- getTime
  result <- insertBy AccountId $ Account (unId aid) Nothing (Just nonce)
  return $ isRight result

generatePasswordResetToken :: (PersistBackend m, MonadSign m) => Id Account -> m (Signed PasswordResetToken)
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

login :: (PersistBackend m) => (Id Account -> m loginInfo) -> Id Account -> Text -> m (Maybe loginInfo)
login toLoginInfo aid password = runMaybeT $ do
  a <- MaybeT $ getBy $ fromId aid
  ph <- MaybeT $ return $ account_passwordHash a
  guard $ verifyPasswordWith pbkdf2 (2^) (encodeUtf8 password) ph
  lift $ toLoginInfo aid

generateAndSendPasswordResetEmail
  :: (PersistBackend m, MonadEmail m, MonadRoute r m, MonadSign m, MonadBrand m)
  => (Signed PasswordResetToken -> Id Account -> m ())
  -> Id Account
  -> m UTCTime
generateAndSendPasswordResetEmail pwEmail aid = do
  nonce <- getTime
  prt <- sign $ PasswordResetToken (aid, nonce)
  pwEmail prt aid
  return nonce

newAccountEmail :: (MonadBrand m, MonadRoute r m, Default r) => (AccountRoute -> r) -> Signed PasswordResetToken -> m Html
newAccountEmail f token = do
  passwordResetLink <- routeToUrl $ f $ AccountRoute_PasswordReset token
  b <- getBrand
  emailTemplate (H.text $ "Welcome to " <> _brand_productName b)
                (H.a H.! A.href (fromString $ show passwordResetLink) $ H.text "Click here to verify your email")
                (H.p $ H.text $ _brand_description b)

sendNewAccountEmail :: (MonadRoute r m, Default r, MonadBrand m, MonadEmail m) => (AccountRoute -> r) -> Id Account -> Signed PasswordResetToken -> m ()
sendNewAccountEmail f aid prt = do
  pn <- liftM T.pack getProductName
  body <- newAccountEmail f prt
  sendEmailDefault (unId aid :| []) (pn <> " Verification Email") body

sendPasswordResetEmail :: (MonadBrand m, MonadEmail m, MonadRoute r m, Default r) => (AccountRoute -> r) -> Signed PasswordResetToken -> Id Account -> m ()
sendPasswordResetEmail f prt aid = do
  passwordResetLink <- routeToUrl $ f $ AccountRoute_PasswordReset prt
  pn <- getProductName
  let lead = "You have received this message because you requested that your " <> pn <> " password be reset. Click the link below to create a new password."
      body = H.a H.! A.href (fromString $ show passwordResetLink) $ "Reset Password"
  sendEmailDefault (unId aid :| []) (T.pack pn <> " Password Reset") =<< emailTemplate (H.text (T.pack pn <> " Password Reset")) (H.toHtml lead) body

