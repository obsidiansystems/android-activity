module Focus.JS.Pretty where

import Reflex.Dom
import Focus.Pretty 

prettyDisplay :: (Show a, MonadWidget t m) => Dynamic t a -> m ()
prettyDisplay x = el "pre" $ do
                    p <- mapDyn prettyShow x
                    dynText p
