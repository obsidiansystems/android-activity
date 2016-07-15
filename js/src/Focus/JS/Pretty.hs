{-# LANGUAGE TypeFamilies, FlexibleContexts, OverloadedStrings #-}
module Focus.JS.Pretty where

import Reflex.Dom
import Focus.Pretty
import qualified Data.Text as T

prettyDisplay :: (Show a, DomBuilder t m, PostBuild t m) => Dynamic t a -> m ()
prettyDisplay x = el "pre" $ do
                    p <- mapDyn (T.pack . prettyShow) x
                    dynText p
