{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Focus.Intercom.Common where

import Data.Text (Text)
import Data.Aeson
import Data.Time (UTCTime)
import Data.Int (Int64)

newtype IntercomUserHash = IntercomUserHash { unIntercomUserHash :: Text } deriving (Read, Show, Eq, Ord, ToJSON, FromJSON)

data IntercomUser = IntercomUser
       { _intercomUser_updatedAt :: UTCTime
       , _intercomUser_txtId :: Text
       -- ^ This is not named _id so it doesn't clash with groundhog's auto generated key
       , _intercomUser_email :: Text
       , _intercomUser_sessionCount :: Int64
       , _intercomUser_location :: Text
       , _intercomUser_userAgent :: Text
       , _intercomUser_device :: Text
       , _intercomUser_browser :: Text
       , _intercomUser_phone :: Text
       }
  deriving (Show, Read, Eq, Ord)
