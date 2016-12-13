{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, OverloadedStrings #-}
module Focus.Route where

import Focus.Brand

import Control.Monad.Reader
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

runRouteT :: RouteT r m a -> RouteEnv -> m a
runRouteT = runReaderT . unRouteT

instance (Monad m, ToJSON r, Default r, Eq r) => MonadRoute r (RouteT r m) where
  routeToUrl r = do
    (baseProto, baseHost, basePort) <- RouteT ask
    let base = URI baseProto (Just $ URIAuth "" baseHost basePort) "/"
    return $ base (routeToQuery r) "" --TODO: https

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
