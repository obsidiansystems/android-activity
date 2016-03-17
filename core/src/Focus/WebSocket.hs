{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
module Focus.WebSocket where

import Control.Lens
import Data.Aeson
import Data.Typeable
import GHC.Generics

import Focus.AppendMap

data WebSocketData auth d t = WebSocketData_Listen (AppendMap auth d) | WebSocketData_Api Value t
  deriving (Eq, Show, Typeable, Generic)

instance (ToJSON auth, ToJSON d, ToJSON t) => ToJSON (WebSocketData auth d t)
instance (Ord auth, FromJSON auth, FromJSON d, FromJSON t) => FromJSON (WebSocketData auth d t)

makePrisms ''WebSocketData
