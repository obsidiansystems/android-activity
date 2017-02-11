{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#else
{-# LANGUAGE LambdaCase #-}
#endif
module Focus.WebSocket where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens
#else
import Control.Lens (Prism, Prism', prism)
#endif
import Data.Aeson
import Data.Typeable
import Data.Text (Text)
import GHC.Generics

data WebSocketData d t = WebSocketData_Listen d
                       | WebSocketData_Api Value t
                       | WebSocketData_Version Text
  deriving (Eq, Show, Typeable, Generic)

instance (ToJSON d, ToJSON t) => ToJSON (WebSocketData d t )
instance (FromJSON d, FromJSON t) => FromJSON (WebSocketData d t)

#ifdef USE_TEMPLATE_HASKELL
makePrisms ''WebSocketData
#else
_WebSocketData_Listen :: Prism (WebSocketData d1 t) (WebSocketData d2 t) d1 d2
_WebSocketData_Listen = prism WebSocketData_Listen $ \case
  WebSocketData_Listen d    -> Right d
  WebSocketData_Api val t   -> Left (WebSocketData_Api val t)
  WebSocketData_Version ver -> Left (WebSocketData_Version ver)
_WebSocketData_Api :: Prism (WebSocketData d t1) (WebSocketData d t2) (Value, t1) (Value, t2)
_WebSocketData_Api = prism (\ (val, t) -> WebSocketData_Api val t) $ \case
  WebSocketData_Listen d -> Left (WebSocketData_Listen d)
  WebSocketData_Api val t -> Right (val, t)
  WebSocketData_Version ver -> Left (WebSocketData_Version ver)
_WebSocketData_Version :: Prism' (WebSocketData d t) Text
_WebSocketData_Version = prism WebSocketData_Version $ \case
  WebSocketData_Version ver -> Right ver
  x -> Left x
#endif
