{-# LANGUAGE TypeFamilies, FlexibleContexts, OverloadedStrings #-}
module Focus.JS.FontAwesome where

import Data.Map (Map)
import Reflex.Dom
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

fontAwesomeCDN :: MonadWidget t m => m ()
fontAwesomeCDN = elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css") $ return ()

dynIconAttr :: MonadWidget t m => Map Text Text -> Dynamic t Text -> m ()
dynIconAttr m i = do
  attr <- forDyn i (\name -> m <> if T.null name then mempty else "class" =: ("fa fa-" <> name))
  elDynAttr "i" attr $ return ()

dynIcon2xAttr :: MonadWidget t m => Map Text Text -> Dynamic t Text -> m ()
dynIcon2xAttr m i = do
  attr <- forDyn i (\name -> m <> if T.null name then mempty else "class" =: ("fa fa-2x fa-" <> name))
  elDynAttr "i" attr $ return ()

dynIcon :: MonadWidget t m => Dynamic t Text -> m ()
dynIcon = dynIconAttr mempty

dynIcon2x :: MonadWidget t m => Dynamic t Text -> m ()
dynIcon2x = dynIcon2xAttr mempty

icon :: MonadWidget t m => Text -> m ()
icon i = elClass "i" ("fa fa-" <> i) $ return ()

icon2x :: MonadWidget t m => Text -> m ()
icon2x i = icon (i <> " fa-2x")
