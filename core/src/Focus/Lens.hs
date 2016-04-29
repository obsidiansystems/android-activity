module Focus.Lens where

import Language.Haskell.TH
import Control.Lens.Internal.FieldTH
import Control.Lens.TH

makeFieldOptics' :: LensRules -> Name -> DecsQ
makeFieldOptics' rules tyName = do
 info <- reify tyName
 case info of
       TyConI dec -> makeFieldOpticsForDec rules dec
       FamilyI _ dataInstances -> fmap (concat . sequence) $ mapM (makeFieldOpticsForDec rules) dataInstances
       _          -> fail "makeFieldOptics: Expected type constructor name"

makeLenses' :: Name -> DecsQ
makeLenses' = makeFieldOptics' lensRules

