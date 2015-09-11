{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies #-}
module Focus.Route where

import Focus.Account
import Focus.Brand
import Focus.Sign

import Focus.Request
import Network.URI
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Aeson
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Default

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
