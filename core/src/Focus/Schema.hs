{-# LANGUAGE StandaloneDeriving, DefaultSignatures, TypeFamilies, FlexibleContexts, UndecidableInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Focus.Schema where

import Data.Aeson
import Data.Aeson.Types
import Data.Int
import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

class HasId a where
  type IdData a :: *
  type IdData a = Int64

newtype Id a = Id { unId :: IdData a } deriving Typeable

deriving instance Read (IdData a) => Read (Id a)
deriving instance Show (IdData a) => Show (Id a)
deriving instance Eq (IdData a) => Eq (Id a)
deriving instance Ord (IdData a) => Ord (Id a)
deriving instance FromJSON (IdData a) => FromJSON (Id a)
deriving instance ToJSON (IdData a) => ToJSON (Id a)

data IdValue a = IdValue (Id a) a deriving Typeable

instance ShowPretty a => ShowPretty (IdValue a) where
  showPretty (IdValue _ x) = showPretty x

instance (Ord k, FromJSON k, FromJSON v) => FromJSON (Map k v) where
  parseJSON = parseJSONMap

parseJSONMap :: (Ord k, FromJSON k, FromJSON v) => Value -> Parser (Map k v)
parseJSONMap v = Map.fromList <$> parseJSON v

instance (ToJSON k, ToJSON v) => ToJSON (Map k v) where
  toJSON = toJSONMap

toJSONMap :: (ToJSON k, ToJSON v) => Map k v -> Value
toJSONMap = toJSON . Map.toList

instance Show (IdData a) => ShowPretty (Id a) where
  showPretty = T.pack . show . unId

class ShowPretty a where
  showPretty :: a -> Text
  default showPretty :: Show a => a -> Text
  showPretty = T.pack . show

type Email = Text --TODO: Validation

-- | Wrapper for storing objects as JSON in the DB. Import the instance from
-- focus-backend:Focus.Backend.Schema
newtype Json a = Json { unJson :: a }
  deriving (Eq, Ord, Show, Read, ToJSON, FromJSON)
