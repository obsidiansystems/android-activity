{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Focus.Intercom.Common where

import Data.Text (Text)
import Data.Aeson

-- COMMON
newtype IntercomUserHash = IntercomUserHash { unIntercomUserHash :: Text } deriving (Read, Show, Eq, Ord, ToJSON, FromJSON)
