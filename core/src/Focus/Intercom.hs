{-# LANGUAGE RecordWildCards, TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Focus.Intercom where

import Control.Exception
import Control.Lens (makeLenses)
import Data.Maybe (fromMaybe)
import Data.Aeson.Compat (decode)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Digest.Pure.SHA (showDigest, hmacSha256)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX
import Network.HTTP.Conduit

import Focus.Intercom.Common (IntercomUserHash(..), IntercomUser(..))
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

-- Same as IntercomUserInternal, but with a different (non-automatic) JSON instance
-- This JSON instance must match the JSON from the Intercom API
newtype IntercomUserInternal = IntercomUserInternal { unIntercomUserInternal :: IntercomUser }
  deriving (Show, Read, Eq, Ord)
instance FromJSON IntercomUserInternal where
  parseJSON = withObject "IntercomUser" $ \o -> do
    _intercomUser_updatedAt <- fmap posixSecondsToUTCTime (o .: "updated_at")
    _intercomUser_txtId <- o .: "id"
    _intercomUser_email <- o .: "email"
    _intercomUser_phone <- fromMaybe "Unknown" <$> o .:? "phone"
    _intercomUser_sessionCount <- o .: "session_count"
    _intercomUser_location <- do
      locationData <- o .: "location_data"
      country <- fromMaybe "" <$> locationData .:? "country_name"
      city <- fromMaybe "" <$> locationData .:? "city_name"
      return $ city <> ", " <> country
    _intercomUser_userAgent <- fromMaybe "Unknown" <$> o .:? "user_agent_data"
    let _intercomUser_device = getDevice _intercomUser_userAgent
    let _intercomUser_browser = getBrowser _intercomUser_userAgent
    return $ IntercomUserInternal $ IntercomUser {..}
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
getIntercomUsers env = getIntercomUsers' Nothing []
  where
    url = "https://api.intercom.io/users/scroll"
    getReq env' mScrollParam = do
      man <- newManager tlsManagerSettings
      req <- fmap (asJson . applyBasicAuth user pass) $ parseUrlThrow $ maybe url ((url <> "?scroll_param=") <>) mScrollParam
      res <- try $ httpLbs req man
      case res of
        Left (err :: HttpException) -> do
          putStrLn $ "ERROR getIntercomUsers: " <> show err
          return mempty
        Right res' -> return $ responseBody res'
      where
        user = T.encodeUtf8 $ _intercomEnv_appId env'
        pass = T.encodeUtf8 $ unIntercomSecretKey $ _intercomEnv_secretKey env'
        asJson req = req { requestHeaders = ("Accept", "application/json"):requestHeaders req }

    getIntercomUsers' :: Maybe String -> [IntercomUser] -> IO [IntercomUser]
    getIntercomUsers' mScrollParam prev = do
      res <- getReq env mScrollParam
      case decode res of
        Nothing -> return prev
        Just body -> do
          let newMScrollParam = either error Just $ parseEither (body .:) "scroll_param"
          let users = either error id $ parseEither (body .:) "users"
          if null users then return prev else getIntercomUsers' newMScrollParam (map unIntercomUserInternal users <> prev)

conversationsUrlFromId :: Text -> Text -> Text
conversationsUrlFromId appId userId =
  "https://app.intercom.io/a/apps/" <> appId <> "/users/" <> userId <> "/all-conversations"
