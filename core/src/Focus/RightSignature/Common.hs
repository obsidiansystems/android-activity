{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Focus.RightSignature.Common where

import Data.Text (Text)

import Focus.Request (makeJson)

-- COMMON
-- These details are generated on the fly using the RightSignature API, and are always temporary
data W9DocumentDetails = W9DocumentDetails
  { _w9DocumentDetails_originalUrl :: Text
  , _w9DocumentDetails_signedUrl :: Text
  , _w9DocumentDetails_thumbnailUrl :: Text
  , _w9DocumentDetails_state :: Text
  } deriving (Show, Read, Eq, Ord)

makeJson ''W9DocumentDetails
