{-# LANGUAGE RecordWildCards, TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Focus.RightSignature where

import Control.Exception
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Data.Vector as V

import Focus.RightSignature.Common

newtype RightSignatureSecretToken = RightSignatureSecretToken { unRightSignatureSecretToken :: Text } deriving (Show, Read, Eq, Ord, FromJSON, ToJSON)

prepackageTemplate :: RightSignatureSecretToken -> Text -> IO (Maybe Text)
prepackageTemplate tok templateId = do
  let url = "https://rightsignature.com/api/templates/" <> T.unpack templateId <> "/prepackage.json"
  res <- makeReq url
  return $ parseMaybe guidParser =<< parseMaybe templateParser =<< decode res
  where
    templateParser = withObject "template" (.: "template")
    guidParser = withObject "guid" (.: "guid")
    makeReq url = do
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
  res <- makeReq url
  return $ parseMaybe guidParser =<< parseMaybe documentParser =<< decode res
  where
    documentParser = withObject "document" (.: "document")
    guidParser = withObject "guid" (.: "guid")
    makeReq url = do
      man <- newManager tlsManagerSettings
      req' <- parseUrlThrow url
      let req = addSecureToken tok $ req' { method = methodPost, requestBody = RequestBodyBS $ T.encodeUtf8 xmlBody, requestHeaders = [("Content-type", "application/xml")] }
      res <- try $ httpLbs req man
      case res of
        Left (err :: HttpException) -> do
          putStrLn $ "Network error: prefillDocument: " <> show err
          return mempty
        Right res' -> return $ responseBody res'

createOneOffDocument :: RightSignatureSecretToken -> Text -> IO (Maybe Text)
createOneOffDocument tok xmlBody = do
  let url = "https://rightsignature.com/api/documents.json"
  res <- makeReq url
  return $ parseMaybe guidParser =<< parseMaybe documentParser =<< decode res
  where
    documentParser = withObject "document" (.: "document")
    guidParser = withObject "guid" (.: "guid")
    makeReq url = do
      man <- newManager tlsManagerSettings
      req' <- parseUrlThrow url
      let req = addSecureToken tok $ req' { method = methodPost, requestBody = RequestBodyBS $ T.encodeUtf8 xmlBody, requestHeaders = [("Content-type", "application/xml")] }
      res <- try $ httpLbs req man
      case res of
        Left (err :: HttpException) -> do
          putStrLn $ "Network error: createOneOffDocument: " <> show err
          return mempty
        Right res' -> return $ responseBody res'

getSignerToken :: RightSignatureSecretToken -> Text -> IO (Maybe Text)
getSignerToken tok guid = do
  let url = "https://rightsignature.com/api/documents/" <> T.unpack guid <> "/signer_links.json"
  res <- makeReq url
  return $ parseMaybe firstSignerLinkParser =<< parseMaybe signerLinksParser =<< parseMaybe documentParser =<< decode res
  where
    documentParser = withObject "document" (.: "document")
    signerLinksParser = withObject "signer_links" (.: "signer_links")
    firstSignerLinkParser = withArray "array" $ \arr -> signerTokenParser (V.head arr)
    signerTokenParser = withObject "signer_token" (.: "signer_token")
    makeReq url = do
      man <- newManager tlsManagerSettings
      req' <- parseUrlThrow url
      let req = addSecureToken tok req'
      res <- try $ httpLbs req man
      case res of
        Left (err :: HttpException) -> do
          putStrLn $ "Network error: getSignerToken: " <> show err
          return mempty
        Right res' -> return $ responseBody res'

getDocumentDetails :: RightSignatureSecretToken -> Text -> IO (Maybe W9DocumentDetails)
getDocumentDetails tok guid = do
  let url = "https://rightsignature.com/api/documents/" <> T.unpack guid <> ".json"
  res <- makeReq url
  return $ do
    doc <- parseMaybe documentParser =<< decode res
    W9DocumentDetails <$> unescapeUrl (parseMaybe (withObject "original_url" (.: "original_url")) doc)
                      <*> unescapeUrl (parseMaybe (withObject "signed_pdf_url" (.: "signed_pdf_url")) doc)
                      <*> unescapeUrl (parseMaybe (withObject "large_url" (.: "large_url")) doc)
                      <*> parseMaybe (withObject "state" (.: "state")) doc
  where
    unescapeUrl = fmap (T.decodeUtf8 . urlDecode False . T.encodeUtf8)
    documentParser = withObject "document" (.: "document")
    makeReq url = do
      man <- newManager tlsManagerSettings
      req' <- parseUrlThrow url
      let req = addSecureToken tok req'
      res <- try $ httpLbs req man
      case res of
        Left (err :: HttpException) -> do
          putStrLn $ "Network error: getDocumentDetails: " <> show err
          return mempty
        Right res' -> return $ responseBody res'

deleteDocument :: RightSignatureSecretToken -> Text -> IO (Maybe Text)
deleteDocument tok guid = do
  let url = "https://rightsignature.com/api/documents/" <> T.unpack guid <> "/trash.json"
  res <- makeReq url
  return $ parseMaybe statusParser =<< parseMaybe documentParser =<< decode res
  where
    documentParser = withObject "document" (.: "document")
    statusParser = withObject "status" (.: "status")
    makeReq url = do
      man <- newManager tlsManagerSettings
      req' <- parseUrlThrow url
      let req = addSecureToken tok $ req' { method = methodPost }
      res <- try $ httpLbs req man
      case res of
        Left (err :: HttpException) -> do
          putStrLn $ "Network error: deleteDocument: " <> show err
          return mempty
        Right res' -> return $ responseBody res'


makeEmbeddedSigningUrl :: Text -> Text
makeEmbeddedSigningUrl signerToken = "https://rightsignature.com/signatures/embedded?height=800&rt=" <> signerToken

addSecureToken :: RightSignatureSecretToken -> Request -> Request
addSecureToken secretTok req =
  req { requestHeaders = ("api-token", T.encodeUtf8 tok) : (requestHeaders req) }
  where
    tok = unRightSignatureSecretToken secretTok
