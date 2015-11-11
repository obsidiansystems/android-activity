{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies #-}
module Focus.Route where

import Focus.Brand

import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import qualified Data.Text as T
import Data.Monoid
import Data.Text.Encoding
import Network.URI

class Monad m => MonadRoute r m | m -> r where
  routeToUrl :: r -> m URI

type RouteEnv = (String, String, String)

newtype RouteT r m a = RouteT { unRouteT :: ReaderT RouteEnv m a } deriving (Functor, Applicative, Monad, MonadIO, MonadBrand)

runRouteT :: RouteT r m a -> RouteEnv -> m a
runRouteT = runReaderT . unRouteT

instance (Monad m, ToJSON r, Default r, Eq r) => MonadRoute r (RouteT r m) where
  routeToUrl r = do
    (baseProto, baseHost, basePort) <- RouteT ask
    let base = URI baseProto (Just $ URIAuth "" baseHost basePort) "/"
    return $ if r == def
             then base "" ""
             else base ("?x=" <> (T.unpack $ decodeUtf8 $ LBS.toStrict $ encode r)) "" --TODO: https

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
