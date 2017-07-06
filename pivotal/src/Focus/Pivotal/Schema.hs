{-# LANGUAGE TemplateHaskell #-}
module Focus.Pivotal.Schema where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)

data TrackerResource
   = TrackerResource { _trackerResource_kind :: Text
                     , _trackerResource_id :: Int
                     , _trackerResource_name :: Text
                     , _trackerResource_story_type :: Text
                     , _trackerResource_url :: Text
                     }
                     deriving (Show, Read, Eq, Ord)

deriveJSON defaultOptions { fieldLabelModifier = drop 17 } ''TrackerResource

data TrackerProject
   = TrackerProject { _trackerProject_kind :: Text
                    , _trackerProject_id :: Int
                    , _trackerProject_name :: Text
                    }
                    deriving (Show, Read, Eq, Ord)

deriveJSON defaultOptions { fieldLabelModifier = drop 16 } ''TrackerProject

data TrackerActivity
   = TrackerActivity { _trackerActivity_kind :: Text
                     , _trackerActivity_guid :: Text
                     , _trackerActivity_project_version :: Int
                     , _trackerActivity_message :: Text
                     , _trackerActivity_highlight :: Text
                     , _trackerActivity_changes :: Value
                     , _trackerActivity_primary_resources :: [TrackerResource]
                     , _trackerActivity_project :: TrackerProject
                     , _trackerActivity_performed_by :: Value
                     }
                     deriving (Show, Read, Eq)

deriveJSON defaultOptions { fieldLabelModifier = drop 17 } ''TrackerActivity
