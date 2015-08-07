module Focus.JS.FontAwesome where

import Reflex.Dom
import Data.Monoid

fontAwesomeCDN :: MonadWidget t m => m ()
fontAwesomeCDN = elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css") $ return ()

icon :: MonadWidget t m => String -> m ()
icon i = elClass "i" ("fa fa-" <> i) $ return ()

icon2x :: MonadWidget t m => String -> m ()
icon2x i = icon (i <> " fa-2x")
