module Focus.JS.PrettyPrint where

import Reflex.Dom
import Language.Haskell.Exts

prettyShow :: (Show a) => a -> String
prettyShow x = let s = show x
               in case parseExp s of
                    ParseOk e -> prettyPrint e
                    ParseFailed _ _ -> s -- just fall back to show

prettyDisplay :: (Show a, MonadWidget t m) => Dynamic t a -> m ()
prettyDisplay x = el "pre" $ do
                    p <- mapDyn prettyShow x
                    dynText p
