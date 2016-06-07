{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Focus.GoogleAnalytics where

import Data.Monoid ((<>))
import Data.Text (Text)
import Control.Lens (makeLenses)
import Focus.Request (makeJson)

data GoogleAnalyticsEnv = GoogleAnalyticsEnv { _googleAnalyticsEnv_trackingId :: Text }

googleAnalyticsScript :: GoogleAnalyticsEnv -> Text
googleAnalyticsScript env = "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){ (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o), m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m) })(window,document,'script','https://www.google-analytics.com/analytics.js','ga'); ga('create', '" <> (_googleAnalyticsEnv_trackingId env) <> "', 'auto'); ga('send', 'pageview');"

makeJson ''GoogleAnalyticsEnv
makeLenses ''GoogleAnalyticsEnv

