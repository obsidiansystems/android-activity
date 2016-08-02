{-# LANGUAGE OverloadedStrings, LambdaCase, TypeFamilies, FlexibleContexts #-}
module Focus.JS.Highlight where

import Focus.Highlight
import Control.Monad
import Data.Text (Text)
import Reflex.Dom

dynHighlightedText :: (DomBuilder t m, PostBuild t m, MonadHold t m)
                   => Dynamic t HighlightedText
                   -> m (Dynamic t Bool) -- ^ Whether any active highlights exist in this text
dynHighlightedText hs = do
  on <- dyn $ ffor hs $ \hs' -> fmap or $ forM hs' $ \case
    Highlight_On t -> (el "mark" $ text t) >> return True
    Highlight_Off t -> (text t) >> return False
  holdDyn False on

dynHighlightedTextQ :: (DomBuilder t m, PostBuild t m, MonadHold t m)
                    => (Text -> Text -> HighlightedText) -- ^ Highlighter
                    -> Dynamic t Text -- ^ Query
                    -> Dynamic t Text -- ^ Text to search 
                    -> m (Dynamic t Bool) -- ^ Whether any of the text is highlighted
dynHighlightedTextQ highlighter query t = dynHighlightedText $ zipDynWith highlighter query t
