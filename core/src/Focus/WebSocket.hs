{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
module Focus.WebSocket where

import Control.Lens
import Data.Aeson
import Data.Typeable
import GHC.Generics

data WebSocketData d t = WebSocketData_Listen d | WebSocketData_Api Value t
  deriving (Eq, Show, Typeable, Generic)

instance (ToJSON d, ToJSON t) => ToJSON (WebSocketData d t)
instance (FromJSON d, FromJSON t) => FromJSON (WebSocketData d t)

makePrisms ''WebSocketData
