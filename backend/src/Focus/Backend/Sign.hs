{-# LANGUAGE TemplateHaskell, FlexibleInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, UndecidableInstances, OverloadedStrings #-}
module Focus.Backend.Sign where

import Focus.Sign
import Focus.Brand
import Focus.Route
import Focus.Request
import Focus.Backend.Email
import Focus.Backend.TH

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Reader
import qualified Web.ClientSession as CS
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Data.Typeable
import Data.Text.Encoding
import Control.Monad.Trans.Maybe

signWithKey k (v :: b) = do
  liftIO $ liftM (Signed . decodeUtf8) $ CS.encryptIO k $ LBS.toStrict $ encode (show $ typeOf (undefined :: b), v)

readSignedWithKey :: (Typeable a, FromJSON a) => CS.Key -> Signed a -> Maybe a
readSignedWithKey k s = do
    tvJson <- CS.decrypt k $ encodeUtf8 $ unSigned s
    (t, v :: b) <- decodeValue' $ LBS.fromStrict tvJson
    guard $ t == show (typeOf (undefined :: b))
    return v

newtype SignT m a = SignT { unSignT :: ReaderT CS.Key m a } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadEmail, MonadRoute r, MonadBrand)

runSignT (SignT a) r = runReaderT a r

instance (MonadIO m, MonadBase IO m) => MonadBase IO (SignT m) where
  liftBase = liftIO

instance MonadTransControl SignT where
  type StT SignT a = StT (ReaderT CS.Key) a
  liftWith f = SignT $ liftWith $ \g -> f $ g . unSignT
  restoreT a = SignT $ restoreT a

instance (MonadIO m, MonadBaseControl IO m) => MonadBaseControl IO (SignT m) where
  type StM (SignT m) a = StM (ReaderT CS.Key m) a
  liftBaseWith f = SignT $ liftBaseWith $ \g -> f $ g . unSignT
  restoreM a = SignT $ restoreM a

instance MonadIO m => MonadSign (SignT m) where
  sign a = do
    k <- SignT ask
    signWithKey k a
  readSigned s = do
    k <- SignT ask
    return $ readSignedWithKey k s

instance MonadSign m => MonadSign (MaybeT m) where
  sign = lift . sign
  readSigned = lift . readSigned

deriveNewtypePersistBackend (\m -> [t| SignT $m |]) (\m -> [t| ReaderT CS.Key $m |]) 'SignT 'unSignT
