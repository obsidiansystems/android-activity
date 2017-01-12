{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Focus.Route where

import Focus.Brand

import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Base
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import qualified Data.Text as T
import Data.Text.Encoding
import Network.URI
import Network.HTTP.Types.URI (renderQuery)

class Monad m => MonadRoute r m | m -> r where
  routeToUrl :: r -> m URI

type RouteEnv = (String, String, String) -- (protocol, hostname, anything after hostname (e.g., port))

newtype RouteT r m a = RouteT { unRouteT :: ReaderT RouteEnv m a } deriving (Functor, Applicative, Monad, MonadIO, MonadBrand, MonadTrans)

instance MonadTransControl (RouteT r) where
    type StT (RouteT r) a = a
    liftWith f = RouteT . ReaderT $ \r -> f $ \t -> runRouteT t r
    restoreT = RouteT . ReaderT . const
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadBase b m => MonadBase b (RouteT r m) where
  liftBase = lift . liftBase

instance (MonadBaseControl b m) => MonadBaseControl b (RouteT r m) where
  type StM (RouteT r m) a = ComposeSt (RouteT r) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

runRouteT :: RouteT r m a -> RouteEnv -> m a
runRouteT = runReaderT . unRouteT

instance (Monad m, ToJSON r, Default r, Eq r) => MonadRoute r (RouteT r m) where
  routeToUrl r = do
    routeEnv <- RouteT ask
    return $ routeToUrlDefault routeEnv r

routeToUrlDefault :: (ToJSON r, Default r, Eq r)
                  => RouteEnv
                  -> r
                  -> URI
routeToUrlDefault (baseProto, baseHost, basePort) r =
  let base = URI baseProto (Just $ URIAuth "" baseHost basePort) "/"
  in base (routeToQuery r) "" --TODO: https


routeToQuery :: (ToJSON r, Default r, Eq r) => r -> String
routeToQuery r = if r == def
  then ""
  else T.unpack . decodeUtf8 $ renderQuery True [("x", Just $ LBS.toStrict $ encode r)]

instance MonadRoute r m => MonadRoute r (ReaderT a m) where
  routeToUrl r = lift $ routeToUrl r

newtype SubRouteT r r' m a = SubRouteT (ReaderT (r' -> r) m a) deriving (Functor, Applicative, Monad, MonadIO, MonadBrand)

instance (MonadRoute r m) => MonadRoute r' (SubRouteT r r' m) where
  routeToUrl r = SubRouteT $ do
    routeConv <- ask
    lift $ routeToUrl $ routeConv r

instance MonadTrans (SubRouteT r r') where
  lift = SubRouteT . lift

runSubRouteT :: SubRouteT r r' m a -> (r' -> r) -> m a
runSubRouteT (SubRouteT a) f = runReaderT a f
