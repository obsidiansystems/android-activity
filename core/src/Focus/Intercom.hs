{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Focus.Intercom where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Lens (makeLenses)
import Data.Aeson
import Focus.Request (makeJson)
import Focus.Schema (Id(..))
import Focus.Account (Account(..))
import qualified Data.ByteString.Lazy as LBS

import Data.Digest.Pure.SHA (showDigest, hmacSha256)

import Focus.Intercom.Common (IntercomUserHash(..))

newtype IntercomSecretKey = IntercomSecretKey { unIntercomSecretKey :: Text } deriving (Show, Read, Eq, Ord, FromJSON, ToJSON)

data IntercomEnv = IntercomEnv
  { _intercomEnv_appId :: Text
  , _intercomEnv_secretKey :: IntercomSecretKey
  } deriving (Show, Read, Eq, Ord)

makeJson ''IntercomEnv
makeLenses ''IntercomEnv

intercomScript :: IntercomEnv -> Text
intercomScript env = "(function(){var w=window;var ic=w.Intercom;w.intercom_app_id='" <> (_intercomEnv_appId env) <> "';if(typeof ic==='function'){ic('reattach_activator');ic('update',intercomSettings);}else{var d=document;var i=function(){i.c(arguments)};i.q=[];i.c=function(args){i.q.push(args)};w.Intercom=i;function l(){var s=d.createElement('script');s.type='text/javascript';s.async=true;s.src='https://widget.intercom.io/widget/'+w.intercom_app_id;var x=d.getElementsByTagName('script')[0];x.parentNode.insertBefore(s,x);}if(w.attachEvent){w.attachEvent('onload',l);}else{w.addEventListener('load',l,false);}}})()"

-- Hash an account id with the intercom secret key
-- NOTE: We use account id. But support for email can also be added
hashIntercom :: IntercomSecretKey -> Id Account -> IntercomUserHash
hashIntercom (IntercomSecretKey intercomSecretKey) aid = IntercomUserHash $ T.pack $ showDigest $ hmacSha256 (LBS.fromStrict $ encodeUtf8 intercomSecretKey) $ LBS.fromStrict $ encodeUtf8 $ T.pack $ show $ unId aid
