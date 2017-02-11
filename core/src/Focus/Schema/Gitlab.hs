{-# LANGUAGE CPP #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
module Focus.Schema.Gitlab where

import Data.Aeson.TH
import Data.Text (Text)

#if !defined(USE_TEMPLATE_HASKELL) || defined(DUMPING_SPLICES)
import Data.Aeson
import Data.Aeson.Encoding.Internal
import Data.Text (pack)
import Data.List (intersperse)
#endif

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

#ifdef USE_TEMPLATE_HASKELL
$(deriveJSON defaultOptions { fieldLabelModifier = drop 9 } ''Project)
#endif

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

#ifdef USE_TEMPLATE_HASKELL
$(deriveJSON defaultOptions { fieldLabelModifier = drop 12 } ''Repository)
#endif

data Author
   = Author { _author_name :: Text
            , _author_email :: Maybe Text
            }
            deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
$(deriveJSON defaultOptions { fieldLabelModifier = drop 8 } ''Author)
#endif

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

#ifdef USE_TEMPLATE_HASKELL
$(deriveJSON defaultOptions { fieldLabelModifier = drop 8 } ''Commit)
#endif

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

#ifdef USE_TEMPLATE_HASKELL
$(deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''Push)
#endif

#ifndef USE_TEMPLATE_HASKELL
--src/Focus/Schema/Gitlab.hs:38:3-69: Splicing declarations
--deriveJSON (defaultOptions {fieldLabelModifier = drop 9}) ''Project
--  ======>
instance ToJSON Project where
  toJSON
    = \ value_a1Ep2
        -> case value_a1Ep2 of {
             Project arg1_a1EpB
                     arg2_a1EpC
                     arg3_a1EpD
                     arg4_a1EpE
                     arg5_a1EpF
                     arg6_a1EpG
                     arg7_a1EpH
                     arg8_a1EpI
                     arg9_a1EpJ
                     arg10_a1EpK
                     arg11_a1EpL
                     arg12_a1EpM
                     arg13_a1EpN
                     arg14_a1EpO
               -> object
                    [keyValuePairWith toJSON (pack "name") arg1_a1EpB,
                     keyValuePairWith toJSON (pack "description") arg2_a1EpC,
                     keyValuePairWith toJSON (pack "web_url") arg3_a1EpD,
                     keyValuePairWith toJSON (pack "avatar_url") arg4_a1EpE,
                     keyValuePairWith toJSON (pack "git_ssh_url") arg5_a1EpF,
                     keyValuePairWith toJSON (pack "git_http_url") arg6_a1EpG,
                     keyValuePairWith toJSON (pack "namespace") arg7_a1EpH,
                     keyValuePairWith toJSON (pack "visibility_level") arg8_a1EpI,
                     keyValuePairWith toJSON (pack "path_with_namespace") arg9_a1EpJ,
                     keyValuePairWith toJSON (pack "default_branch") arg10_a1EpK,
                     keyValuePairWith toJSON (pack "homepage") arg11_a1EpL,
                     keyValuePairWith toJSON (pack "url") arg12_a1EpM,
                     keyValuePairWith toJSON (pack "ssh_url") arg13_a1EpN,
                     keyValuePairWith toJSON (pack "http_url") arg14_a1EpO] }
  toEncoding
    = \ value_a1Eq8
        -> case value_a1Eq8 of {
             Project arg1_a1Eqb
                     arg2_a1Eqc
                     arg3_a1Eqd
                     arg4_a1Eqe
                     arg5_a1Eqf
                     arg6_a1Eqg
                     arg7_a1Eqh
                     arg8_a1Eqi
                     arg9_a1Eqj
                     arg10_a1Eqk
                     arg11_a1Eql
                     arg12_a1Eqm
                     arg13_a1Eqn
                     arg14_a1Eqo
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "name")) >< (colon >< (toEncoding arg1_a1Eqb))),
                           ((text (pack "description"))
                            >< (colon >< (toEncoding arg2_a1Eqc))),
                           ((text (pack "web_url")) >< (colon >< (toEncoding arg3_a1Eqd))),
                           ((text (pack "avatar_url")) >< (colon >< (toEncoding arg4_a1Eqe))),
                           ((text (pack "git_ssh_url"))
                            >< (colon >< (toEncoding arg5_a1Eqf))),
                           ((text (pack "git_http_url"))
                            >< (colon >< (toEncoding arg6_a1Eqg))),
                           ((text (pack "namespace")) >< (colon >< (toEncoding arg7_a1Eqh))),
                           ((text (pack "visibility_level"))
                            >< (colon >< (toEncoding arg8_a1Eqi))),
                           ((text (pack "path_with_namespace"))
                            >< (colon >< (toEncoding arg9_a1Eqj))),
                           ((text (pack "default_branch"))
                            >< (colon >< (toEncoding arg10_a1Eqk))),
                           ((text (pack "homepage")) >< (colon >< (toEncoding arg11_a1Eql))),
                           ((text (pack "url")) >< (colon >< (toEncoding arg12_a1Eqm))),
                           ((text (pack "ssh_url")) >< (colon >< (toEncoding arg13_a1Eqn))),
                           ((text (pack "http_url"))
                            >< (colon >< (toEncoding arg14_a1Eqo)))])) }
instance FromJSON Project where
  parseJSON
    = \ value_a1EqS
        -> case value_a1EqS of
             Object recObj_a1EqV
               -> ((((((((((((((Project
                                <$>
                                  (lookupField
                                     parseJSON
                                     "Focus.Schema.Gitlab.Project"
                                     "Project"
                                     recObj_a1EqV
                                     (pack "name")))
                               <*>
                                 (lookupField
                                    parseJSON
                                    "Focus.Schema.Gitlab.Project"
                                    "Project"
                                    recObj_a1EqV
                                    (pack "description")))
                              <*>
                                (lookupField
                                   parseJSON
                                   "Focus.Schema.Gitlab.Project"
                                   "Project"
                                   recObj_a1EqV
                                   (pack "web_url")))
                             <*>
                               (lookupField
                                  parseJSON
                                  "Focus.Schema.Gitlab.Project"
                                  "Project"
                                  recObj_a1EqV
                                  (pack "avatar_url")))
                            <*>
                              (lookupField
                                 parseJSON
                                 "Focus.Schema.Gitlab.Project"
                                 "Project"
                                 recObj_a1EqV
                                 (pack "git_ssh_url")))
                           <*>
                             (lookupField
                                parseJSON
                                "Focus.Schema.Gitlab.Project"
                                "Project"
                                recObj_a1EqV
                                (pack "git_http_url")))
                          <*>
                            (lookupField
                               parseJSON
                               "Focus.Schema.Gitlab.Project"
                               "Project"
                               recObj_a1EqV
                               (pack "namespace")))
                         <*>
                           (lookupField
                              parseJSON
                              "Focus.Schema.Gitlab.Project"
                              "Project"
                              recObj_a1EqV
                              (pack "visibility_level")))
                        <*>
                          (lookupField
                             parseJSON
                             "Focus.Schema.Gitlab.Project"
                             "Project"
                             recObj_a1EqV
                             (pack "path_with_namespace")))
                       <*>
                         (lookupField
                            parseJSON
                            "Focus.Schema.Gitlab.Project"
                            "Project"
                            recObj_a1EqV
                            (pack "default_branch")))
                      <*>
                        (lookupField
                           parseJSON
                           "Focus.Schema.Gitlab.Project"
                           "Project"
                           recObj_a1EqV
                           (pack "homepage")))
                     <*>
                       (lookupField
                          parseJSON
                          "Focus.Schema.Gitlab.Project"
                          "Project"
                          recObj_a1EqV
                          (pack "url")))
                    <*>
                      (lookupField
                         parseJSON
                         "Focus.Schema.Gitlab.Project"
                         "Project"
                         recObj_a1EqV
                         (pack "ssh_url")))
                   <*>
                     (lookupField
                        parseJSON
                        "Focus.Schema.Gitlab.Project"
                        "Project"
                        recObj_a1EqV
                        (pack "http_url")))
             other_a1Erd
               -> parseTypeMismatch'
                    "Project"
                    "Focus.Schema.Gitlab.Project"
                    "Object"
                    (valueConName other_a1Erd)
--src/Focus/Schema/Gitlab.hs:53:3-73: Splicing declarations
--deriveJSON
--  (defaultOptions {fieldLabelModifier = drop 12}) ''Repository
--  ======>
instance ToJSON Repository where
  toJSON
    = \ value_a1F7O
        -> case value_a1F7O of {
             Repository arg1_a1F7V
                        arg2_a1F7W
                        arg3_a1F7X
                        arg4_a1F7Y
                        arg5_a1F7Z
                        arg6_a1F80
                        arg7_a1F81
               -> object
                    [keyValuePairWith toJSON (pack "name") arg1_a1F7V,
                     keyValuePairWith toJSON (pack "url") arg2_a1F7W,
                     keyValuePairWith toJSON (pack "description") arg3_a1F7X,
                     keyValuePairWith toJSON (pack "homepage") arg4_a1F7Y,
                     keyValuePairWith toJSON (pack "git_http_url") arg5_a1F7Z,
                     keyValuePairWith toJSON (pack "git_ssh_url") arg6_a1F80,
                     keyValuePairWith toJSON (pack "visibility_level") arg7_a1F81] }
  toEncoding
    = \ value_a1F83
        -> case value_a1F83 of {
             Repository arg1_a1F87
                        arg2_a1F88
                        arg3_a1F8a
                        arg4_a1F8b
                        arg5_a1F8c
                        arg6_a1F8d
                        arg7_a1F8e
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "name")) >< (colon >< (toEncoding arg1_a1F87))),
                           ((text (pack "url")) >< (colon >< (toEncoding arg2_a1F88))),
                           ((text (pack "description"))
                            >< (colon >< (toEncoding arg3_a1F8a))),
                           ((text (pack "homepage")) >< (colon >< (toEncoding arg4_a1F8b))),
                           ((text (pack "git_http_url"))
                            >< (colon >< (toEncoding arg5_a1F8c))),
                           ((text (pack "git_ssh_url"))
                            >< (colon >< (toEncoding arg6_a1F8d))),
                           ((text (pack "visibility_level"))
                            >< (colon >< (toEncoding arg7_a1F8e)))])) }
instance FromJSON Repository where
  parseJSON
    = \ value_a1F8B
        -> case value_a1F8B of
             Object recObj_a1F8D
               -> (((((((Repository
                         <$>
                           (lookupField
                              parseJSON
                              "Focus.Schema.Gitlab.Repository"
                              "Repository"
                              recObj_a1F8D
                              (pack "name")))
                        <*>
                          (lookupField
                             parseJSON
                             "Focus.Schema.Gitlab.Repository"
                             "Repository"
                             recObj_a1F8D
                             (pack "url")))
                       <*>
                         (lookupField
                            parseJSON
                            "Focus.Schema.Gitlab.Repository"
                            "Repository"
                            recObj_a1F8D
                            (pack "description")))
                      <*>
                        (lookupField
                           parseJSON
                           "Focus.Schema.Gitlab.Repository"
                           "Repository"
                           recObj_a1F8D
                           (pack "homepage")))
                     <*>
                       (lookupField
                          parseJSON
                          "Focus.Schema.Gitlab.Repository"
                          "Repository"
                          recObj_a1F8D
                          (pack "git_http_url")))
                    <*>
                      (lookupField
                         parseJSON
                         "Focus.Schema.Gitlab.Repository"
                         "Repository"
                         recObj_a1F8D
                         (pack "git_ssh_url")))
                   <*>
                     (lookupField
                        parseJSON
                        "Focus.Schema.Gitlab.Repository"
                        "Repository"
                        recObj_a1F8D
                        (pack "visibility_level")))
             other_a1F9l
               -> parseTypeMismatch'
                    "Repository"
                    "Focus.Schema.Gitlab.Repository"
                    "Object"
                    (valueConName other_a1F9l)
--src/Focus/Schema/Gitlab.hs:63:3-68: Splicing declarations
--deriveJSON (defaultOptions {fieldLabelModifier = drop 8}) ''Author
--  ======>
instance ToJSON Author where
  toJSON
    = \ value_a1FD5
        -> case value_a1FD5 of {
             Author arg1_a1FD6 arg2_a1FD7
               -> object
                    [keyValuePairWith toJSON (pack "name") arg1_a1FD6,
                     keyValuePairWith toJSON (pack "email") arg2_a1FD7] }
  toEncoding
    = \ value_a1FD8
        -> case value_a1FD8 of {
             Author arg1_a1FD9 arg2_a1FDa
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "name")) >< (colon >< (toEncoding arg1_a1FD9))),
                           ((text (pack "email")) >< (colon >< (toEncoding arg2_a1FDa)))])) }
instance FromJSON Author where
  parseJSON
    = \ value_a1FDb
        -> case value_a1FDb of
             Object recObj_a1FDc
               -> ((Author
                    <$>
                      (lookupField
                         parseJSON
                         "Focus.Schema.Gitlab.Author"
                         "Author"
                         recObj_a1FDc
                         (pack "name")))
                   <*>
                     (lookupField
                        parseJSON
                        "Focus.Schema.Gitlab.Author"
                        "Author"
                        recObj_a1FDc
                        (pack "email")))
             other_a1FDd
               -> parseTypeMismatch'
                    "Author"
                    "Focus.Schema.Gitlab.Author"
                    "Object"
                    (valueConName other_a1FDd)
--src/Focus/Schema/Gitlab.hs:79:3-68: Splicing declarations
--deriveJSON (defaultOptions {fieldLabelModifier = drop 8}) ''Commit
--  ======>
instance ToJSON Commit where
  toJSON
    = \ value_a1G0u
        -> case value_a1G0u of {
             Commit arg1_a1G0D
                    arg2_a1G0E
                    arg3_a1G0F
                    arg4_a1G0G
                    arg5_a1G0H
                    arg6_a1G0I
                    arg7_a1G0J
                    arg8_a1G0K
               -> object
                    [keyValuePairWith toJSON (pack "id") arg1_a1G0D,
                     keyValuePairWith toJSON (pack "message") arg2_a1G0E,
                     keyValuePairWith toJSON (pack "timestamp") arg3_a1G0F,
                     keyValuePairWith toJSON (pack "url") arg4_a1G0G,
                     keyValuePairWith toJSON (pack "author") arg5_a1G0H,
                     keyValuePairWith toJSON (pack "added") arg6_a1G0I,
                     keyValuePairWith toJSON (pack "modified") arg7_a1G0J,
                     keyValuePairWith toJSON (pack "removed") arg8_a1G0K] }
  toEncoding
    = \ value_a1G0M
        -> case value_a1G0M of {
             Commit arg1_a1G0N
                    arg2_a1G0O
                    arg3_a1G0P
                    arg4_a1G0Q
                    arg5_a1G0R
                    arg6_a1G0S
                    arg7_a1G0T
                    arg8_a1G0U
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "id")) >< (colon >< (toEncoding arg1_a1G0N))),
                           ((text (pack "message")) >< (colon >< (toEncoding arg2_a1G0O))),
                           ((text (pack "timestamp")) >< (colon >< (toEncoding arg3_a1G0P))),
                           ((text (pack "url")) >< (colon >< (toEncoding arg4_a1G0Q))),
                           ((text (pack "author")) >< (colon >< (toEncoding arg5_a1G0R))),
                           ((text (pack "added")) >< (colon >< (toEncoding arg6_a1G0S))),
                           ((text (pack "modified")) >< (colon >< (toEncoding arg7_a1G0T))),
                           ((text (pack "removed"))
                            >< (colon >< (toEncoding arg8_a1G0U)))])) }
instance FromJSON Commit where
  parseJSON
    = \ value_a1G13
        -> case value_a1G13 of
             Object recObj_a1G14
               -> ((((((((Commit
                          <$>
                            (lookupField
                               parseJSON
                               "Focus.Schema.Gitlab.Commit"
                               "Commit"
                               recObj_a1G14
                               (pack "id")))
                         <*>
                           (lookupField
                              parseJSON
                              "Focus.Schema.Gitlab.Commit"
                              "Commit"
                              recObj_a1G14
                              (pack "message")))
                        <*>
                          (lookupField
                             parseJSON
                             "Focus.Schema.Gitlab.Commit"
                             "Commit"
                             recObj_a1G14
                             (pack "timestamp")))
                       <*>
                         (lookupField
                            parseJSON
                            "Focus.Schema.Gitlab.Commit"
                            "Commit"
                            recObj_a1G14
                            (pack "url")))
                      <*>
                        (lookupField
                           parseJSON
                           "Focus.Schema.Gitlab.Commit"
                           "Commit"
                           recObj_a1G14
                           (pack "author")))
                     <*>
                       (lookupField
                          parseJSON
                          "Focus.Schema.Gitlab.Commit"
                          "Commit"
                          recObj_a1G14
                          (pack "added")))
                    <*>
                      (lookupField
                         parseJSON
                         "Focus.Schema.Gitlab.Commit"
                         "Commit"
                         recObj_a1G14
                         (pack "modified")))
                   <*>
                     (lookupField
                        parseJSON
                        "Focus.Schema.Gitlab.Commit"
                        "Commit"
                        recObj_a1G14
                        (pack "removed")))
             other_a1G19
               -> parseTypeMismatch'
                    "Commit"
                    "Focus.Schema.Gitlab.Commit"
                    "Object"
                    (valueConName other_a1G19)
--src/Focus/Schema/Gitlab.hs:103:3-66: Splicing declarations
--deriveJSON (defaultOptions {fieldLabelModifier = drop 6}) ''Push
--  ======>
instance ToJSON Push where
  toJSON
    = \ value_a1GL1
        -> case value_a1GL1 of {
             Push arg1_a1GLC
                  arg2_a1GLD
                  arg3_a1GLE
                  arg4_a1GLF
                  arg5_a1GLG
                  arg6_a1GLI
                  arg7_a1GLJ
                  arg8_a1GLK
                  arg9_a1GLL
                  arg10_a1GLM
                  arg11_a1GLN
                  arg12_a1GLO
                  arg13_a1GLP
                  arg14_a1GLQ
               -> object
                    [keyValuePairWith toJSON (pack "object_kind") arg1_a1GLC,
                     keyValuePairWith toJSON (pack "before") arg2_a1GLD,
                     keyValuePairWith toJSON (pack "after") arg3_a1GLE,
                     keyValuePairWith toJSON (pack "ref") arg4_a1GLF,
                     keyValuePairWith toJSON (pack "checkout_sha") arg5_a1GLG,
                     keyValuePairWith toJSON (pack "user_id") arg6_a1GLI,
                     keyValuePairWith toJSON (pack "user_name") arg7_a1GLJ,
                     keyValuePairWith toJSON (pack "user_email") arg8_a1GLK,
                     keyValuePairWith toJSON (pack "user_avatar") arg9_a1GLL,
                     keyValuePairWith toJSON (pack "project_id") arg10_a1GLM,
                     keyValuePairWith toJSON (pack "project") arg11_a1GLN,
                     keyValuePairWith toJSON (pack "repository") arg12_a1GLO,
                     keyValuePairWith toJSON (pack "commits") arg13_a1GLP,
                     keyValuePairWith toJSON (pack "total_commits_count") arg14_a1GLQ] }
  toEncoding
    = \ value_a1GM9
        -> case value_a1GM9 of {
             Push arg1_a1GMa
                  arg2_a1GMb
                  arg3_a1GMc
                  arg4_a1GMd
                  arg5_a1GMe
                  arg6_a1GMf
                  arg7_a1GMg
                  arg8_a1GMh
                  arg9_a1GMi
                  arg10_a1GMj
                  arg11_a1GMk
                  arg12_a1GMl
                  arg13_a1GMm
                  arg14_a1GMn
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "object_kind"))
                            >< (colon >< (toEncoding arg1_a1GMa))),
                           ((text (pack "before")) >< (colon >< (toEncoding arg2_a1GMb))),
                           ((text (pack "after")) >< (colon >< (toEncoding arg3_a1GMc))),
                           ((text (pack "ref")) >< (colon >< (toEncoding arg4_a1GMd))),
                           ((text (pack "checkout_sha"))
                            >< (colon >< (toEncoding arg5_a1GMe))),
                           ((text (pack "user_id")) >< (colon >< (toEncoding arg6_a1GMf))),
                           ((text (pack "user_name")) >< (colon >< (toEncoding arg7_a1GMg))),
                           ((text (pack "user_email")) >< (colon >< (toEncoding arg8_a1GMh))),
                           ((text (pack "user_avatar"))
                            >< (colon >< (toEncoding arg9_a1GMi))),
                           ((text (pack "project_id"))
                            >< (colon >< (toEncoding arg10_a1GMj))),
                           ((text (pack "project")) >< (colon >< (toEncoding arg11_a1GMk))),
                           ((text (pack "repository"))
                            >< (colon >< (toEncoding arg12_a1GMl))),
                           ((text (pack "commits")) >< (colon >< (toEncoding arg13_a1GMm))),
                           ((text (pack "total_commits_count"))
                            >< (colon >< (toEncoding arg14_a1GMn)))])) }
instance FromJSON Push where
  parseJSON
    = \ value_a1GMZ
        -> case value_a1GMZ of
             Object recObj_a1GN1
               -> ((((((((((((((Push
                                <$>
                                  (lookupField
                                     parseJSON
                                     "Focus.Schema.Gitlab.Push"
                                     "Push"
                                     recObj_a1GN1
                                     (pack "object_kind")))
                               <*>
                                 (lookupField
                                    parseJSON
                                    "Focus.Schema.Gitlab.Push"
                                    "Push"
                                    recObj_a1GN1
                                    (pack "before")))
                              <*>
                                (lookupField
                                   parseJSON
                                   "Focus.Schema.Gitlab.Push"
                                   "Push"
                                   recObj_a1GN1
                                   (pack "after")))
                             <*>
                               (lookupField
                                  parseJSON
                                  "Focus.Schema.Gitlab.Push"
                                  "Push"
                                  recObj_a1GN1
                                  (pack "ref")))
                            <*>
                              (lookupField
                                 parseJSON
                                 "Focus.Schema.Gitlab.Push"
                                 "Push"
                                 recObj_a1GN1
                                 (pack "checkout_sha")))
                           <*>
                             (lookupField
                                parseJSON
                                "Focus.Schema.Gitlab.Push"
                                "Push"
                                recObj_a1GN1
                                (pack "user_id")))
                          <*>
                            (lookupField
                               parseJSON
                               "Focus.Schema.Gitlab.Push"
                               "Push"
                               recObj_a1GN1
                               (pack "user_name")))
                         <*>
                           (lookupField
                              parseJSON
                              "Focus.Schema.Gitlab.Push"
                              "Push"
                              recObj_a1GN1
                              (pack "user_email")))
                        <*>
                          (lookupField
                             parseJSON
                             "Focus.Schema.Gitlab.Push"
                             "Push"
                             recObj_a1GN1
                             (pack "user_avatar")))
                       <*>
                         (lookupField
                            parseJSON
                            "Focus.Schema.Gitlab.Push"
                            "Push"
                            recObj_a1GN1
                            (pack "project_id")))
                      <*>
                        (lookupField
                           parseJSON
                           "Focus.Schema.Gitlab.Push"
                           "Push"
                           recObj_a1GN1
                           (pack "project")))
                     <*>
                       (lookupField
                          parseJSON
                          "Focus.Schema.Gitlab.Push"
                          "Push"
                          recObj_a1GN1
                          (pack "repository")))
                    <*>
                      (lookupField
                         parseJSON
                         "Focus.Schema.Gitlab.Push"
                         "Push"
                         recObj_a1GN1
                         (pack "commits")))
                   <*>
                     (lookupField
                        parseJSON
                        "Focus.Schema.Gitlab.Push"
                        "Push"
                        recObj_a1GN1
                        (pack "total_commits_count")))
             other_a1GNK
               -> parseTypeMismatch'
                    "Push"
                    "Focus.Schema.Gitlab.Push"
                    "Object"
                    (valueConName other_a1GNK)
#endif
