{-# LANGUAGE ScopedTypeVariables #-}
module Focus.JS.Widget where

import Reflex.Dom
import qualified Data.Map as Map

enumDropdown :: forall t m k. (MonadWidget t m, Enum k, Bounded k, Show k, Read k, Ord k) => (k -> String) -> DropdownConfig t k -> m (Dropdown t k)
enumDropdown f cfg = do
  let xs = [minBound .. maxBound] :: [k]
      xMap = Map.fromList $ zip xs (map f xs)
  dropdown minBound (constDyn xMap) cfg


