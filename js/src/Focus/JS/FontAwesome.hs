{-# LANGUAGE TypeFamilies, FlexibleContexts, OverloadedStrings #-}
module Focus.JS.FontAwesome where

import Data.Map (Map)
import Reflex.Dom
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

fontAwesomeCDN :: DomBuilder t m => m ()
fontAwesomeCDN = elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css") $ return ()

dynIconAttr :: (DomBuilder t m, PostBuild t m) => Map Text Text -> Dynamic t Text -> m ()
dynIconAttr m i = do
  let attr = ffor i $ \name -> m <> if T.null name then mempty else "class" =: ("fa fa-" <> name)
  elDynAttr "i" attr $ return ()

dynIcon2xAttr :: (DomBuilder t m, PostBuild t m) => Map Text Text -> Dynamic t Text -> m ()
dynIcon2xAttr m i = do
  let attr = ffor i $ \name -> m <> if T.null name then mempty else "class" =: ("fa fa-2x fa-" <> name)
  elDynAttr "i" attr $ return ()

dynIcon :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m ()
dynIcon = dynIconAttr mempty

dynIcon2x :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m ()
dynIcon2x = dynIcon2xAttr mempty

icon :: DomBuilder t m => Text -> m ()
icon i = elClass "i" ("fa fa-" <> i) $ return ()

icon2x :: DomBuilder t m => Text -> m ()
icon2x i = icon (i <> " fa-2x")
