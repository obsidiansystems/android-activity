{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
module Focus.RightSignature.Common where

import Data.Text (Text)

#ifdef USE_TEMPLATE_HASKELL
import Focus.Request (makeJson)
#endif

#if !defined(USE_TEMPLATE_HASKELL) || defined(DUMPING_SPLICES)
import Focus.Request (HList(..))
import Data.Aeson
#endif

-- COMMON
-- These details are generated on the fly using the RightSignature API, and are always temporary
data W9DocumentDetails = W9DocumentDetails
  { _w9DocumentDetails_originalUrl :: Text
  , _w9DocumentDetails_signedUrl :: Text
  , _w9DocumentDetails_thumbnailUrl :: Text
  , _w9DocumentDetails_state :: Text
  } deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
makeJson ''W9DocumentDetails
#else
instance ToJSON W9DocumentDetails where
  toJSON r_a3pV3
    = case r_a3pV3 of {
        W9DocumentDetails f_a3pV5 f_a3pV6 f_a3pV7 f_a3pV8
          -> toJSON
               ("W9DocumentDetails" :: String,
              toJSON
                (HCons
                   f_a3pV5 (HCons f_a3pV6 (HCons f_a3pV7 (HCons f_a3pV8 HNil))))) }
instance FromJSON W9DocumentDetails where
  parseJSON v_a3pV9
    = do { (tag'_a3pVb, v'_a3pVc) <- parseJSON v_a3pV9;
           case tag'_a3pVb :: String of
             "W9DocumentDetails"
               -> do { HCons f_a3pVd
                             (HCons f_a3pVe (HCons f_a3pVf (HCons f_a3pVg HNil))) <- parseJSON
                                                                                       v'_a3pVc;
                       return (W9DocumentDetails f_a3pVd f_a3pVe f_a3pVf f_a3pVg) }
             _ -> fail "invalid message" }
#endif
