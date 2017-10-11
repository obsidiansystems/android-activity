{-# LANGUAGE TemplateHaskell, FlexibleInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, UndecidableInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Focus.Backend.Sign where

import Focus.Backend.TH
import Focus.Brand
import Focus.Email
import Focus.Request
import Focus.Route
import Focus.Sign

import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding
import Data.Typeable
import Database.Groundhog
import qualified Web.ClientSession as CS

-- Web.ClientSession just uses "normal" base-64 encoding, which includes
-- the characters '+' and '/'.  Which makes them not suitable for using
-- as part of a URL.  There is a URL safe base-64 encoding which instead
-- uses the characters '-' and '_' (respectively). So after the fact we
-- "fix" this.  We could decode the URL-unsafe version and re-encode it
-- as URL-safe, or we could just map the characters.  Guess which is more
-- efficient.

convertBase64ToURLSafe :: BS8.ByteString -> BS8.ByteString
convertBase64ToURLSafe = BS8.map go
    where
        go :: Char -> Char
        go '+' = '-'
        go '/' = '_'
        go '-' = error "convertBase64ToURLSafe: say a '-'!"
        go '_' = error "convertBase64ToURLSafe: say a '_'!"
        go x   = x  -- everything else is unchanged

convertBase64ToURLUnsafe :: BS8.ByteString -> BS8.ByteString
convertBase64ToURLUnsafe = BS8.map go
    where
        go :: Char -> Char
        go '-' = '+'
        go '_' = '/'
        go '+' = error "convertBase64ToURLUnsafe: say a '+'!"
        go '/' = error "convertBase64ToURLUnsafe: say a '/'!"
        go x   = x  -- everything else is unchanged

signWithKey :: (Typeable b, ToJSON b, MonadIO m) => CS.Key -> b -> m (Signed a)
signWithKey k (v :: b) = do
  liftIO $ liftM (Signed . decodeUtf8 . convertBase64ToURLSafe) $ CS.encryptIO k $ LBS.toStrict $ encode (show $ typeOf (undefined :: b), v)

readSignedWithKey :: (Typeable a, FromJSON a) => CS.Key -> Signed a -> Maybe a
readSignedWithKey k s = do
    tvJson <- CS.decrypt k $ convertBase64ToURLUnsafe $ encodeUtf8 $ unSigned s
    (t, v :: b) <- decodeValue' $ LBS.fromStrict tvJson
    guard $ t == show (typeOf (undefined :: b))
    return v

newtype SignT m a = SignT { unSignT :: ReaderT CS.Key m a } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadEmail, MonadRoute r, MonadBrand)

runSignT :: SignT m a -> CS.Key -> m a
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

instance MonadSign m => MonadSign (NoLoggingT m) where
  sign = lift . sign
  readSigned = lift . readSigned

instance MonadSign m => MonadSign (DbPersist conn m) where
  sign = lift . sign
  readSigned = lift . readSigned

deriveNewtypePersistBackend (\m -> [t| SignT $m |]) (\m -> [t| ReaderT CS.Key $m |]) 'SignT 'unSignT
