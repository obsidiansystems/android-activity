{-# LANGUAGE EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Data.Indexed.Projections where

import Data.Indexed
import Control.Lens ((&), (.~), at)
import Data.Proxy
import Data.Map

data Id

instance Projection Id a where
  type Projected Id a = a
  project _ = id

data Length

instance Foldable a => Projection Length (a v) where
  type Projected Length (a v) = Int
  project _ = length

indexed_test :: WithIndex Length (Map Int String)
indexed_test = withIndex (Proxy :: Proxy Length) mempty
  & at 5 .~ Just ("Hello, world!" :: String)
  & at 6 .~ Just "asdfasdfasdfa"
  & at 1 .~ Just "a"

