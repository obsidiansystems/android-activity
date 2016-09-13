{-# LANGUAGE TupleSections, OverloadedStrings, TemplateHaskell #-}
module Focus.Backend.Auth where

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (decodeLenient)
import Data.Word8 (isSpace, _colon, toLower)
import Snap

import Control.Lens (makeLenses)
import Focus.Request (makeJson)

-- Currently only supports single user simple http auth
data HttpAuthSettings
  = HttpAuthSettings_NoAuth
  | HttpAuthSettings_SimpleAuth
    { _httpAuthSettings_username :: Text
    , _httpAuthSettings_password :: Text
    , _httpAuthSettings_realm :: Text
    }

makeJson ''HttpAuthSettings
makeLenses ''HttpAuthSettings

withHttpAuth :: HttpAuthSettings -> Snap () -> Snap ()
withHttpAuth HttpAuthSettings_NoAuth w = w
withHttpAuth (HttpAuthSettings_SimpleAuth u p r) w = do
  req <- getRequest
  rawHeader <- maybe requestAuth return $ getHeader "Authorization" req
  extracted <- maybe accessDenied return $ extractBasicAuth rawHeader
  if extracted == (toByteString u, toByteString p) then w else accessDenied
  where
    requestAuth = do
      modifyResponse $ setHeader "WWW-Authenticate" (BS.append "Basic realm=" $ toByteString r)
      modifyResponse $ setResponseCode 401
      stopProcessingNow
    accessDenied = do
      modifyResponse $ setResponseCode 403
      stopProcessingNow

    -- Snap API is quite inadequate. This should really be a builtin
    -- Once this is called, nothing else in the handler will run
    stopProcessingNow = getResponse >>= finishWith

-- Adapted from: https://github.com/yesodweb/wai/blob/master/wai-extra/Network/Wai/Middleware/HttpAuth.hs#L103
extractBasicAuth :: ByteString -> Maybe (ByteString, ByteString)
extractBasicAuth bs =
    let (x, y) = BS.break isSpace bs
    in if BS.map toLower x == "basic"
       then extract $ BS.dropWhile isSpace y
       else Nothing
  where
    extract encoded =
        let raw = decodeLenient encoded
            (username, password') = BS.break (== _colon) raw
        in ((username,) . snd) <$> BS.uncons password'

toByteString :: Text -> ByteString
toByteString = T.encodeUtf8
