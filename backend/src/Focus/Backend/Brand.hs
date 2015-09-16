{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeFamilies, UndecidableInstances #-}
module Focus.Backend.Brand ( module Focus.Backend.Brand
                           , module Focus.Brand
                           ) where

import Focus.Brand
import Control.Monad.Trans
import Control.Monad.Reader
import Focus.Backend.TH

deriveNewtypePersistBackend (\m -> [t| BrandT $m |]) (\m -> [t| ReaderT Brand $m |]) 'BrandT 'unBrandT
