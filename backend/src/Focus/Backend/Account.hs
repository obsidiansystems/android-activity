{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Focus.Backend.Account where

import Focus.Backend.DB
import Focus.Backend.Email
import Focus.Backend.Schema.TH
import Focus.Backend.Listen

import Focus.Account
import Focus.Brand
import Focus.Sign
import Focus.Route
import Focus.Schema

import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Crypto.PasswordStore
import Data.Default
import Data.Maybe
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
    constructors:
      - name: Account
        uniques:
          - name: emailUnique
            type: constraint
            fields: [account_email]
|]

makeDefaultKeyIdInt64 ''Account 'AccountKey

migrateAccount :: PersistBackend m => Migration m
migrateAccount = migrate (undefined :: Account)

-- Returns whether a new account had to be created
ensureAccountExists :: (PersistBackend m) => Email -> m (Maybe (Id Account)) 
ensureAccountExists email = do
  nonce <- getTime
  result <- insertByAll $ Account email Nothing (Just nonce)
  case result of
    Left _ -> return Nothing
    Right aid -> do
      let aid' = toId aid
      notifyEntityId NotificationType_Insert aid'
      return (Just aid')

-- Creates account if it doesn't already exist and sends pw email
ensureAccountExistsEmail
  :: (PersistBackend m, MonadSign m)
  => (Signed PasswordResetToken -> Email -> m ()) -- pw reset email
  -> Email
  -> m (Maybe (Id Account))
ensureAccountExistsEmail pwEmail email = do
  maid <- ensureAccountExists email
  forM_ maid $ \aid -> do
    mNonce <- generateAndSendPasswordResetEmail pwEmail aid
    forM_ mNonce $ \nonce -> do
      update [Account_passwordResetNonceField =. Just nonce] (Account_emailField ==. email)
  return maid

generatePasswordResetToken :: (PersistBackend m, MonadSign m) => Id Account -> m (Signed PasswordResetToken)
generatePasswordResetToken aid = do
  nonce <- getTime
  sign $ PasswordResetToken (aid, nonce)

setAccountPassword :: (PersistBackend m, MonadIO m) => Id Account -> Text -> m ()
setAccountPassword aid password = do
  salt <- liftIO genSaltIO
  update [ Account_passwordHashField =. Just (makePasswordSaltWith pbkdf2 (2^) (encodeUtf8 password) salt 14)
         , Account_passwordResetNonceField =. (Nothing :: Maybe UTCTime) ]
         (AutoKeyField ==. fromId aid)

setPasswordWithToken :: (MonadIO m, PersistBackend m, MonadSign m) => Signed AuthToken -> Text -> m ()
setPasswordWithToken token password = do
  Just (AuthToken aid) <- readSigned token
  setAccountPassword aid password

resetPasswordWithToken :: (MonadIO m, PersistBackend m, MonadSign m) => Signed PasswordResetToken -> Text -> m (Id Account)
resetPasswordWithToken prt password = do
  Just (PasswordResetToken (aid, nonce)) <- readSigned prt
  Just a <- get $ fromId aid
  True <- return $ account_passwordResetNonce a == Just nonce
  setAccountPassword aid password
  return aid

login :: (PersistBackend m) => (Id Account -> m loginInfo) -> Email -> Text -> m (Maybe loginInfo)
login toLoginInfo email password = runMaybeT $ do
  (aid, a) <- MaybeT . fmap listToMaybe $ project (AutoKeyField, AccountConstructor) (Account_emailField ==. email) 
  ph <- MaybeT . return $ account_passwordHash a
  guard $ verifyPasswordWith pbkdf2 (2^) (encodeUtf8 password) ph
  lift $ toLoginInfo (toId aid)

generateAndSendPasswordResetEmail
  :: (PersistBackend m, MonadSign m)
  => (Signed PasswordResetToken -> Email -> m ())
  -> Id Account 
  -> m (Maybe UTCTime)
generateAndSendPasswordResetEmail pwEmail aid = do
  nonce <- getTime
  prt <- sign $ PasswordResetToken (aid, nonce)
  ma <- get (fromId aid)
  forM ma $ \a -> do
    pwEmail prt (account_email a)
    return nonce

newAccountEmail :: (MonadBrand m, MonadRoute r m, Default r) => (AccountRoute -> r) -> Signed PasswordResetToken -> m Html
newAccountEmail f token = do
  passwordResetLink <- routeToUrl $ f $ AccountRoute_PasswordReset token
  b <- getBrand
  emailTemplate Nothing
                (H.text $ "Welcome to " <> _brand_productName b)
                (H.a H.! A.href (fromString $ show passwordResetLink) $ H.text "Click here to verify your email")
                (H.p $ H.text $ _brand_description b)

sendNewAccountEmail :: (MonadRoute r m, Default r, MonadBrand m, MonadEmail m)
                    => (AccountRoute -> r) -- How to turn AccountRoute into a route for a specific app
                    -> Signed PasswordResetToken
                    -> Email
                    -> m ()
sendNewAccountEmail f prt email = do
  pn <- liftM T.pack getProductName
  body <- newAccountEmail f prt
  sendEmailDefault (email :| []) (pn <> " Verification Email") body

sendPasswordResetEmail :: (MonadBrand m, MonadEmail m, MonadRoute r m, Default r) => (AccountRoute -> r) -> Signed PasswordResetToken -> Email -> m ()
sendPasswordResetEmail f prt email = do
  passwordResetLink <- routeToUrl $ f $ AccountRoute_PasswordReset prt
  pn <- getProductName
  let lead = "You have received this message because you requested that your " <> pn <> " password be reset. Click the link below to create a new password."
      body = H.a H.! A.href (fromString $ show passwordResetLink) $ "Reset Password"
  sendEmailDefault (email :| []) (T.pack pn <> " Password Reset") =<< emailTemplate Nothing (H.text (T.pack pn <> " Password Reset")) (H.toHtml lead) body

