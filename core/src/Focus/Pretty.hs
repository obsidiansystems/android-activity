module Focus.Pretty where

import Language.Haskell.Exts

prettyShow :: (Show a) => a -> String
prettyShow x = let s = show x
               in case parseExp s of
                    ParseOk e -> prettyPrint e
                    ParseFailed _ _ -> s -- just fall back to show

