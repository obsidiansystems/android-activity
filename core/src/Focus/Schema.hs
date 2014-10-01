{-# LANGUAGE StandaloneDeriving, DefaultSignatures, TypeFamilies, FlexibleContexts, UndecidableInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Focus.Schema where

import Data.Aeson
import Data.Int
import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative

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

instance (Ord k, FromJSON k, FromJSON v) => FromJSON (Map k v) where
  parseJSON v = Map.fromList <$> parseJSON v

instance (ToJSON k, ToJSON v) => ToJSON (Map k v) where
  toJSON = toJSON . Map.toList

instance (Show (IdData a)) => ShowPretty (Id a) where
  showPretty = show . unId

class ShowPretty a where
  showPretty :: a -> String
  default showPretty :: Show a => a -> String
  showPretty = show
