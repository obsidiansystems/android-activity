{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleInstances #-}
module Focus.Backend.Route ( module Focus.Route
                           ) where

import Focus.Route
{-
import Control.Monad.Trans
import Control.Monad.Reader
import Focus.Backend.TH

deriveNewtypePersistBackend (\m -> [t| RouteT Route $m |]) (\m -> [t| ReaderT RouteEnv $m |]) 'RouteT 'unRouteT
-}
