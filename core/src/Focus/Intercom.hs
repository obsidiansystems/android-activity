{-# LANGUAGE RecordWildCards, TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Focus.Intercom where

import Control.Lens (makeLenses)
import Data.Aeson
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

import Focus.Intercom.Common (IntercomUserHash(..))
import Focus.Request (makeJson)


newtype IntercomSecretKey = IntercomSecretKey { unIntercomSecretKey :: Text } deriving (Show, Read, Eq, Ord, FromJSON, ToJSON)

data IntercomEnv = IntercomEnv
  { _intercomEnv_appId :: Text
  , _intercomEnv_secureModeKey :: IntercomSecretKey
  , _intercomEnv_secretKey :: IntercomSecretKey
  } deriving (Show, Read, Eq, Ord)

makeJson ''IntercomEnv
makeLenses ''IntercomEnv

intercomScript :: IntercomEnv -> Text
intercomScript env = "(function(){var w=window;var ic=w.Intercom;w.intercom_app_id='" <> (_intercomEnv_appId env) <> "';if(typeof ic==='function'){ic('reattach_activator');ic('update',intercomSettings);}else{var d=document;var i=function(){i.c(arguments)};i.q=[];i.c=function(args){i.q.push(args)};w.Intercom=i;function l(){var s=d.createElement('script');s.type='text/javascript';s.async=true;s.src='https://widget.intercom.io/widget/'+w.intercom_app_id;var x=d.getElementsByTagName('script')[0];x.parentNode.insertBefore(s,x);}if(w.attachEvent){w.attachEvent('onload',l);}else{w.addEventListener('load',l,false);}}})()"

-- Hash an account id with the intercom secret key
-- NOTE: We use email. But support for account id can also be added
hashIntercom :: IntercomSecretKey -> Text -> IntercomUserHash
hashIntercom (IntercomSecretKey intercomSecretKey) email = IntercomUserHash $ T.pack $ showDigest $ hmacSha256 (LBS.fromStrict $ T.encodeUtf8 intercomSecretKey) $ LBS.fromStrict $ T.encodeUtf8 email

data IntercomUser = IntercomUser
       { _intercomUser_updatedAt :: UTCTime
       , _intercomUser_id :: Text
       , _intercomUser_email :: Text
       , _intercomUser_sessionCount :: Int64
       , _intercomUser_location :: Text
       , _intercomUser_userAgent :: Text
       , _intercomUser_device :: Text
       , _intercomUser_browser :: Text
       }
  deriving (Show, Read, Eq, Ord)

instance FromJSON IntercomUser where
  parseJSON = withObject "IntercomUser" $ \o -> do
    _intercomUser_updatedAt <- fmap posixSecondsToUTCTime (o .: "updated_at")
    _intercomUser_id <- o .: "id"
    _intercomUser_email <- o .: "email"
    _intercomUser_sessionCount <- o .: "session_count"
    _intercomUser_location <- do
      locationData <- o .: "location_data"
      country <- locationData .: "country_name"
      city <- locationData .: "city_name"
      return $ city <> ", " <> country
    _intercomUser_userAgent <- o .: "user_agent_data"
    let _intercomUser_device = getDevice _intercomUser_userAgent
    let _intercomUser_browser = getBrowser _intercomUser_userAgent
    return IntercomUser {..}
    where
      -- Simplistic user agent sniffing
      getDevice ua
        | "Mobile" `T.isInfixOf` ua = "Mobile"
        | otherwise = "Unknown"
      getBrowser ua
        | "Chrome" `T.isInfixOf` ua = "Chrome"
        | "Safari" `T.isInfixOf` ua = "Safari"
        | ("Opera" `T.isInfixOf` ua) || ("OPR" `T.isInfixOf` ua) = "Opera"
        | "IE" `T.isInfixOf` ua = "Internet Explorer"
        | otherwise = "Unknown"


getIntercomUsers :: IntercomEnv -> IO [IntercomUser]
getIntercomUsers env = do
  let url = "https://api.intercom.io/users"
  res <- getReq env url
  return $ maybe mempty id $ parseMaybe userListParser =<< decode res
  where
    userListParser = withObject "getIntercomUsers" (.: "users")

    getReq env' url = do
      man <- newManager tlsManagerSettings
      req <- fmap (asJson . applyBasicAuth user pass) $ parseUrlThrow url
      res <- httpLbs req man
      return $ responseBody res
      where
        user = T.encodeUtf8 $ _intercomEnv_appId env'
        pass = T.encodeUtf8 $ unIntercomSecretKey $ _intercomEnv_secretKey env'
        asJson req = req { requestHeaders = ("Accept", "application/json"):requestHeaders req }

conversationsUrlFromId :: Text -> Text -> Text
conversationsUrlFromId appId userId =
  "https://app.intercom.io/a/apps/" <> appId <> "/users/" <> userId <> "/all-conversations"
