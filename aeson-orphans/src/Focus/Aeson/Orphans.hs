{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Focus.Aeson.Orphans where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64 as B64
import Data.Monoid hiding (First (..))
import Data.Semigroup
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

instance ToJSON ByteString where
    toJSON = toJSON . decodeUtf8 . B64.encode

instance FromJSON ByteString where
    parseJSON o = either fail return . B64.decode . encodeUtf8 =<< parseJSON o

instance ToJSON LBS.ByteString where
    toJSON = toJSON . decodeUtf8 . B64.encode . LBS.toStrict

instance FromJSON LBS.ByteString where
    parseJSON o = either fail (return . LBS.fromStrict) . B64.decode . encodeUtf8 =<< parseJSON o

deriving instance FromJSON a => FromJSON (First a)
deriving instance ToJSON a => ToJSON (First a)

deriving instance FromJSON Any
deriving instance ToJSON Any

instance (ToJSON (f a)) => ToJSON (Alt f a)
instance (FromJSON (f a)) => FromJSON (Alt f a)
