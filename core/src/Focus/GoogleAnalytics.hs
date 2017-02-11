{-# LANGUAGE CPP, OverloadedStrings #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
module Focus.GoogleAnalytics where

import Data.Monoid ((<>))
import Data.Text (Text)
#ifdef USE_TEMPLATE_HASKELL
import Control.Lens (makeLenses)
import Focus.Request (makeJson)
#else
import Control.Lens (Lens')
#endif
#if !defined(USE_TEMPLATE_HASKELL) || defined(DUMPING_SPLICES)
import Data.Aeson
import Focus.Request (HList(..))
#endif

data GoogleAnalyticsEnv = GoogleAnalyticsEnv { _googleAnalyticsEnv_trackingId :: Text }

googleAnalyticsScript :: GoogleAnalyticsEnv -> Text
googleAnalyticsScript env = "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){ (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o), m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m) })(window,document,'script','https://www.google-analytics.com/analytics.js','ga'); ga('create', '" <> (_googleAnalyticsEnv_trackingId env) <> "', 'auto'); ga('send', 'pageview');"

#ifdef USE_TEMPLATE_HASKELL
makeJson ''GoogleAnalyticsEnv
makeLenses ''GoogleAnalyticsEnv
#else
instance ToJSON GoogleAnalyticsEnv where
  toJSON r_a2q1g
    = case r_a2q1g of {
        GoogleAnalyticsEnv f_a2q1o
          -> toJSON
               ("GoogleAnalyticsEnv" :: String, toJSON (HCons f_a2q1o HNil)) }
instance FromJSON GoogleAnalyticsEnv where
  parseJSON v_a2q1u
    = do { (tag'_a2q1y, v'_a2q1A) <- parseJSON v_a2q1u;
           case tag'_a2q1y :: String of
             "GoogleAnalyticsEnv"
               -> do { HCons f_a2q1D HNil <- parseJSON v'_a2q1A;
                       return (GoogleAnalyticsEnv f_a2q1D) }
             _ -> fail "invalid message" }

googleAnalyticsEnv_trackingId :: Lens' GoogleAnalyticsEnv Text
googleAnalyticsEnv_trackingId f (GoogleAnalyticsEnv a) = (\a' -> GoogleAnalyticsEnv a') <$> f a
{-# INLINE googleAnalyticsEnv_trackingId #-}
#endif

