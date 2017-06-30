{-# LANGUAGE TypeFamilies, FlexibleContexts, OverloadedStrings #-}
module Focus.JS.FontAwesome where

import Data.Map (Map)
import Reflex.Dom.Core
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

import Web.FontAwesomeType -- ^ FontAwesome Enumerations

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

icon1g :: DomBuilder t m => Text -> m ()
icon1g i = icon (i <> " fa-1g")

icon2x :: DomBuilder t m => Text -> m ()
icon2x i = icon (i <> " fa-2x")

icon3x :: DomBuilder t m => Text -> m ()
icon3x i = icon (i <> " fa-3x")

icon4x :: DomBuilder t m => Text -> m ()
icon4x i = icon (i <> " fa-4x")

icon5x :: DomBuilder t m => Text -> m ()
icon5x i = icon (i <> " fa-5x")

faIcon :: DomBuilder t m => FontAwesome -> m ()
faIcon i = elClass "i" ("fa " <> (T.pack (fontAwesomeClass i))) $ return ()

faIcon1g :: DomBuilder t m => FontAwesome -> m ()
faIcon1g i = icon1g $ drop3class i

faIcon2x :: DomBuilder t m => FontAwesome -> m ()
faIcon2x i = icon2x $ drop3class i 

faIcon3x :: DomBuilder t m => FontAwesome -> m ()
faIcon3x i = icon3x $ drop3class i

faIcon4x :: DomBuilder t m => FontAwesome -> m ()
faIcon4x i = icon4x $ drop3class i

faIcon5x :: DomBuilder t m => FontAwesome -> m ()
faIcon5x i = icon5x $ drop3class i

-- helper functions --
drop3class :: FontAwesome -> Text
drop3class = T.drop 3 . T.pack . fontAwesomeClass
