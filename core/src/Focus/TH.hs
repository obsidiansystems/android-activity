module Focus.TH where

import Language.Haskell.TH

conName :: Con -> Name
conName x = case x of
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ c -> conName c
