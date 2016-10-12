{-# LANGUAGE RecordWildCards, TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Focus.RightSignature where

import Control.Exception
import Control.Lens (makeLenses)
import Data.Aeson.Compat
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Digest.Pure.SHA (showDigest, hmacSha256)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Data.Vector as V

import Focus.Request (makeJson)

newtype RightSignatureSecretToken = RightSignatureSecretToken { unRightSignatureSecretToken :: Text } deriving (Show, Read, Eq, Ord, FromJSON, ToJSON)

prepackageTemplate :: RightSignatureSecretToken -> Text -> IO (Maybe Text)
prepackageTemplate tok templateId = do
  let url = "https://rightsignature.com/api/templates/" <> T.unpack templateId <> "/prepackage.json"
  res <- makeReq tok url
  return $ parseMaybe guidParser =<< parseMaybe templateParser =<< decode res
  where
    templateParser = withObject "template" (.: "template")
    guidParser = withObject "guid" (.: "guid")
    makeReq tok url = do
      man <- newManager tlsManagerSettings
      req' <- parseUrlThrow url
      let req = addSecureToken tok $ req' { method = methodPost }
      res <- try $ httpLbs req man
      case res of
        Left (err :: HttpException) -> do
          putStrLn $ "Network error: prepackageTemplate: " <> show err
          return mempty
        Right res' -> return $ responseBody res'

prefillDocument :: RightSignatureSecretToken -> Text -> IO (Maybe Text)
prefillDocument tok xmlBody = do
  let url = "https://rightsignature.com/api/templates.json"
  res <- makeReq tok url
  return $ parseMaybe guidParser =<< parseMaybe documentParser =<< decode res
  where
    documentParser = withObject "document" (.: "document")
    guidParser = withObject "guid" (.: "guid")
    makeReq tok url = do
      man <- newManager tlsManagerSettings
      req' <- parseUrlThrow url
      let req = addSecureToken tok $ req' { method = methodPost, requestBody = RequestBodyBS $ T.encodeUtf8 xmlBody, requestHeaders = [("Content-type", "application/xml")] }
      res <- try $ httpLbs req man
      case res of
        Left (err :: HttpException) -> do
          putStrLn $ "Network error: prefillDocument: " <> show err
          return mempty
        Right res' -> return $ responseBody res'

getSignerToken :: RightSignatureSecretToken -> Text -> IO (Maybe Text)
getSignerToken tok guid = do
  let url = "https://rightsignature.com/api/documents/" <> T.unpack guid <> "/signer_links.json"
  res <- makeReq tok url
  return $ parseMaybe firstSignerLinkParser =<< parseMaybe signerLinksParser =<< parseMaybe documentParser =<< decode res
  where
    documentParser = withObject "document" (.: "document")
    signerLinksParser = withObject "signer_links" (.: "signer_links")
    firstSignerLinkParser = withArray "array" $ \arr -> signerTokenParser (V.head arr)
    signerTokenParser = withObject "signer_token" (.: "signer_token")
    makeReq tok url = do
      man <- newManager tlsManagerSettings
      req' <- parseUrlThrow url
      let req = addSecureToken tok req'
      res <- try $ httpLbs req man
      case res of
        Left (err :: HttpException) -> do
          putStrLn $ "Network error: getSignerToken: " <> show err
          return mempty
        Right res' -> return $ responseBody res'

makeEmbeddedSigningUrl :: Text -> Text
makeEmbeddedSigningUrl signerToken = "https://rightsignature.com/signatures/embedded?height=800&rt=" <> signerToken

addSecureToken :: RightSignatureSecretToken -> Request -> Request
addSecureToken secretTok req =
  req { requestHeaders = ("api-token", T.encodeUtf8 tok) : (requestHeaders req) }
  where
    tok = unRightSignatureSecretToken secretTok
