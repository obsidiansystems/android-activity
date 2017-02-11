{-# LANGUAGE CPP #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
module Focus.Schema.Pivotal where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)

#if !defined(USE_TEMPLATE_HASKELL) || defined(DUMPING_SPLICES)
import Data.Aeson.Encoding.Internal (text, wrapObject, econcat, comma, (><), colon)
import Data.Text (pack)
import Data.List (intersperse)
#endif

data TrackerResource
   = TrackerResource { _trackerResource_kind :: Text
                     , _trackerResource_id :: Int
                     , _trackerResource_name :: Text
                     , _trackerResource_story_type :: Text
                     , _trackerResource_url :: Text
                     }
                     deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
$(deriveJSON defaultOptions { fieldLabelModifier = drop 17 } ''TrackerResource)
#else
instance ToJSON TrackerResource where
  toJSON
    = \ value_aGzz
        -> case value_aGzz of {
             TrackerResource arg1_aGzL arg2_aGzM arg3_aGzN arg4_aGzO arg5_aGzP
               -> object
                    [keyValuePairWith toJSON (pack "kind") arg1_aGzL,
                     keyValuePairWith toJSON (pack "id") arg2_aGzM,
                     keyValuePairWith toJSON (pack "name") arg3_aGzN,
                     keyValuePairWith toJSON (pack "story_type") arg4_aGzO,
                     keyValuePairWith toJSON (pack "url") arg5_aGzP] }
  toEncoding
    = \ value_aGzY
        -> case value_aGzY of {
             TrackerResource arg1_aGA0 arg2_aGA1 arg3_aGA2 arg4_aGA3 arg5_aGA4
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "kind")) >< (colon >< (toEncoding arg1_aGA0))),
                           ((text (pack "id")) >< (colon >< (toEncoding arg2_aGA1))),
                           ((text (pack "name")) >< (colon >< (toEncoding arg3_aGA2))),
                           ((text (pack "story_type")) >< (colon >< (toEncoding arg4_aGA3))),
                           ((text (pack "url")) >< (colon >< (toEncoding arg5_aGA4)))])) }
instance FromJSON TrackerResource where
  parseJSON
    = \ value_aGA6
        -> case value_aGA6 of
             Object recObj_aGA7
               -> (((((TrackerResource
                       <$>
                         (lookupField
                            parseJSON
                            "Focus.Schema.Pivotal.TrackerResource"
                            "TrackerResource"
                            recObj_aGA7
                            (pack "kind")))
                      <*>
                        (lookupField
                           parseJSON
                           "Focus.Schema.Pivotal.TrackerResource"
                           "TrackerResource"
                           recObj_aGA7
                           (pack "id")))
                     <*>
                       (lookupField
                          parseJSON
                          "Focus.Schema.Pivotal.TrackerResource"
                          "TrackerResource"
                          recObj_aGA7
                          (pack "name")))
                    <*>
                      (lookupField
                         parseJSON
                         "Focus.Schema.Pivotal.TrackerResource"
                         "TrackerResource"
                         recObj_aGA7
                         (pack "story_type")))
                   <*>
                     (lookupField
                        parseJSON
                        "Focus.Schema.Pivotal.TrackerResource"
                        "TrackerResource"
                        recObj_aGA7
                        (pack "url")))
             other_aGAj
               -> parseTypeMismatch'
                    "TrackerResource"
                    "Focus.Schema.Pivotal.TrackerResource"
                    "Object"
                    (valueConName other_aGAj)
#endif

data TrackerProject
   = TrackerProject { _trackerProject_kind :: Text
                    , _trackerProject_id :: Int
                    , _trackerProject_name :: Text
                    }
                    deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
$(deriveJSON defaultOptions { fieldLabelModifier = drop 16 } ''TrackerProject)
#else
instance ToJSON TrackerProject where
  toJSON
    = \ value_aHLQ
        -> case value_aHLQ of {
             TrackerProject arg1_aHM4 arg2_aHM5 arg3_aHM7
               -> object
                    [keyValuePairWith toJSON (pack "kind") arg1_aHM4,
                     keyValuePairWith toJSON (pack "id") arg2_aHM5,
                     keyValuePairWith toJSON (pack "name") arg3_aHM7] }
  toEncoding
    = \ value_aHMe
        -> case value_aHMe of {
             TrackerProject arg1_aHMg arg2_aHMh arg3_aHMi
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "kind")) >< (colon >< (toEncoding arg1_aHMg))),
                           ((text (pack "id")) >< (colon >< (toEncoding arg2_aHMh))),
                           ((text (pack "name")) >< (colon >< (toEncoding arg3_aHMi)))])) }
instance FromJSON TrackerProject where
  parseJSON
    = \ value_aHMx
        -> case value_aHMx of
             Object recObj_aHMy
               -> (((TrackerProject
                     <$>
                       (lookupField
                          parseJSON
                          "Focus.Schema.Pivotal.TrackerProject"
                          "TrackerProject"
                          recObj_aHMy
                          (pack "kind")))
                    <*>
                      (lookupField
                         parseJSON
                         "Focus.Schema.Pivotal.TrackerProject"
                         "TrackerProject"
                         recObj_aHMy
                         (pack "id")))
                   <*>
                     (lookupField
                        parseJSON
                        "Focus.Schema.Pivotal.TrackerProject"
                        "TrackerProject"
                        recObj_aHMy
                        (pack "name")))
             other_aHMH
               -> parseTypeMismatch'
                    "TrackerProject"
                    "Focus.Schema.Pivotal.TrackerProject"
                    "Object"
                    (valueConName other_aHMH)
#endif

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

#ifdef USE_TEMPLATE_HASKELL
$(deriveJSON defaultOptions { fieldLabelModifier = drop 17 } ''TrackerActivity)
#else
instance ToJSON TrackerActivity where
  toJSON
    = \ value_aIMI
        -> case value_aIMI of {
             TrackerActivity arg1_aINw
                             arg2_aINx
                             arg3_aINy
                             arg4_aINz
                             arg5_aINA
                             arg6_aINB
                             arg7_aINC
                             arg8_aIND
                             arg9_aINE
               -> object
                    [keyValuePairWith toJSON (pack "kind") arg1_aINw,
                     keyValuePairWith toJSON (pack "guid") arg2_aINx,
                     keyValuePairWith toJSON (pack "project_version") arg3_aINy,
                     keyValuePairWith toJSON (pack "message") arg4_aINz,
                     keyValuePairWith toJSON (pack "highlight") arg5_aINA,
                     keyValuePairWith toJSON (pack "changes") arg6_aINB,
                     keyValuePairWith toJSON (pack "primary_resources") arg7_aINC,
                     keyValuePairWith toJSON (pack "project") arg8_aIND,
                     keyValuePairWith toJSON (pack "performed_by") arg9_aINE] }
  toEncoding
    = \ value_aIOR
        -> case value_aIOR of {
             TrackerActivity arg1_aIOX
                             arg2_aIOY
                             arg3_aIP0
                             arg4_aIP2
                             arg5_aIP3
                             arg6_aIP5
                             arg7_aIP6
                             arg8_aIP7
                             arg9_aIP9
               -> wrapObject
                    (econcat
                       (intersperse
                          comma
                          [((text (pack "kind")) >< (colon >< (toEncoding arg1_aIOX))),
                           ((text (pack "guid")) >< (colon >< (toEncoding arg2_aIOY))),
                           ((text (pack "project_version"))
                            >< (colon >< (toEncoding arg3_aIP0))),
                           ((text (pack "message")) >< (colon >< (toEncoding arg4_aIP2))),
                           ((text (pack "highlight")) >< (colon >< (toEncoding arg5_aIP3))),
                           ((text (pack "changes")) >< (colon >< (toEncoding arg6_aIP5))),
                           ((text (pack "primary_resources"))
                            >< (colon >< (toEncoding arg7_aIP6))),
                           ((text (pack "project")) >< (colon >< (toEncoding arg8_aIP7))),
                           ((text (pack "performed_by"))
                            >< (colon >< (toEncoding arg9_aIP9)))])) }
instance FromJSON TrackerActivity where
  parseJSON
    = \ value_aIT7
        -> case value_aIT7 of
             Object recObj_aIT8
               -> (((((((((TrackerActivity
                           <$>
                             (lookupField
                                parseJSON
                                "Focus.Schema.Pivotal.TrackerActivity"
                                "TrackerActivity"
                                recObj_aIT8
                                (pack "kind")))
                          <*>
                            (lookupField
                               parseJSON
                               "Focus.Schema.Pivotal.TrackerActivity"
                               "TrackerActivity"
                               recObj_aIT8
                               (pack "guid")))
                         <*>
                           (lookupField
                              parseJSON
                              "Focus.Schema.Pivotal.TrackerActivity"
                              "TrackerActivity"
                              recObj_aIT8
                              (pack "project_version")))
                        <*>
                          (lookupField
                             parseJSON
                             "Focus.Schema.Pivotal.TrackerActivity"
                             "TrackerActivity"
                             recObj_aIT8
                             (pack "message")))
                       <*>
                         (lookupField
                            parseJSON
                            "Focus.Schema.Pivotal.TrackerActivity"
                            "TrackerActivity"
                            recObj_aIT8
                            (pack "highlight")))
                      <*>
                        (lookupField
                           parseJSON
                           "Focus.Schema.Pivotal.TrackerActivity"
                           "TrackerActivity"
                           recObj_aIT8
                           (pack "changes")))
                     <*>
                       (lookupField
                          parseJSON
                          "Focus.Schema.Pivotal.TrackerActivity"
                          "TrackerActivity"
                          recObj_aIT8
                          (pack "primary_resources")))
                    <*>
                      (lookupField
                         parseJSON
                         "Focus.Schema.Pivotal.TrackerActivity"
                         "TrackerActivity"
                         recObj_aIT8
                         (pack "project")))
                   <*>
                     (lookupField
                        parseJSON
                        "Focus.Schema.Pivotal.TrackerActivity"
                        "TrackerActivity"
                        recObj_aIT8
                        (pack "performed_by")))
             other_aIWK
               -> parseTypeMismatch'
                    "TrackerActivity"
                    "Focus.Schema.Pivotal.TrackerActivity"
                    "Object"
                    (valueConName other_aIWK)
#endif
