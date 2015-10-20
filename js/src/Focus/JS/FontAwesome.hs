module Focus.JS.FontAwesome where

import Data.Map (Map)
import Reflex.Dom
import Data.Monoid

fontAwesomeCDN :: MonadWidget t m => m ()
fontAwesomeCDN = elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css") $ return ()

dynIconAttr :: MonadWidget t m => Map String String -> Dynamic t String -> m ()
dynIconAttr m i = do attr <- forDyn i (\name -> m <> if null name then mempty else "class" =: ("fa fa-" ++ name))
                     elDynAttr "i" attr $ return ()

dynIcon2xAttr :: MonadWidget t m => Map String String -> Dynamic t String -> m ()
dynIcon2xAttr m i = do attr <- forDyn i (\name -> m <> if null name then mempty else "class" =: ("fa fa-2x fa-" ++ name))
                       elDynAttr "i" attr $ return ()

dynIcon :: MonadWidget t m => Dynamic t String -> m ()
dynIcon = dynIconAttr mempty

dynIcon2x :: MonadWidget t m => Dynamic t String -> m ()
dynIcon2x = dynIcon2xAttr mempty

icon :: MonadWidget t m => String -> m ()
icon i = elClass "i" ("fa fa-" <> i) $ return ()

icon2x :: MonadWidget t m => String -> m ()
icon2x i = icon (i <> " fa-2x")
