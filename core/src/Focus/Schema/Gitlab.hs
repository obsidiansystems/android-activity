{-# LANGUAGE TemplateHaskell #-}
module Focus.Schema.Gitlab where

import Data.Aeson.TH
import Data.Text (Text)

import Focus.Schema

data Project
   = Project { _project_name :: Text
             , _project_description :: Maybe Text
             , _project_web_url ::Maybe  Text
             , _project_avatar_url ::Maybe  Text
             , _project_git_ssh_url :: Text
             , _project_git_http_url :: Text
             , _project_namespace :: Text
             , _project_visibility_level :: Int
             , _project_path_with_namespace :: Text
             , _project_default_branch :: Text
             , _project_homepage ::Maybe  Text
             , _project_url :: Text
             , _project_ssh_url :: Text
             , _project_http_url :: Text
             }
             deriving (Show, Read, Eq, Ord)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 9 } ''Project)

data Repository
   = Repository { _repository_name :: Text
                , _repository_url :: Text
                , _repository_description :: Maybe Text
                , _repository_homepage ::Maybe  Text
                , _repository_git_http_url :: Text
                , _repository_git_ssh_url :: Text
                , _repository_visibility_level :: Int
                }
                deriving (Show, Read, Eq, Ord)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 12 } ''Repository)

data Author
   = Author { _author_name :: Text
            , _author_email :: Maybe Text
            }
            deriving (Show, Read, Eq, Ord)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 8 } ''Author)

data Commit
   = Commit { _commit_id :: Text
            , _commit_message :: Text
            , _commit_timestamp :: Text
            , _commit_url :: Text
            , _commit_author :: Author
            , _commit_added :: [Text]
            , _commit_modified :: [Text]
            , _commit_removed :: [Text]
            }
            deriving (Show, Read, Eq, Ord)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 8 } ''Commit)

data Push
   = Push { _push_object_kind :: Text
          , _push_before :: Text
          , _push_after :: Text
          , _push_ref :: Text
          , _push_checkout_sha :: Text
          , _push_user_id :: Int
          , _push_user_name :: Text
          , _push_user_email :: Maybe Text
          , _push_user_avatar :: Maybe Text
          , _push_project_id :: Int
          , _push_project :: Project
          , _push_repository :: Repository
          , _push_commits :: [Commit]
          , _push_total_commits_count :: Int
          }
          deriving (Show, Read, Eq, Ord)

instance HasId Push

$(deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''Push)
