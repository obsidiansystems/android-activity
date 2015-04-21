{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving #-}
module Focus.Route where

import Focus.Account
import Focus.Brand

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

data Route
  = Route_Index
  | Route_ResetPassword (Signed PasswordResetToken)
  deriving (Show, Read, Eq, Ord)
makeJson ''Route

class Monad m => MonadRoute m where
  routeToUrl :: Route -> m URI

type RouteEnv = (String, String, String)

newtype RouteT m a = RouteT { unRouteT :: ReaderT RouteEnv m a } deriving (Functor, Applicative, Monad, MonadIO, MonadBrand)

runRouteT :: RouteT m a -> RouteEnv -> m a
runRouteT = runReaderT . unRouteT

instance Monad m => MonadRoute (RouteT m) where
  routeToUrl r = do
    (baseProto, baseHost, basePort) <- RouteT ask
    let base = URI baseProto (Just $ URIAuth "" baseHost basePort) "/"
    return $ case r of
      Route_Index -> base "" ""
      _ -> base ("?x=" <> (T.unpack $ decodeUtf8 $ LBS.toStrict $ encode r)) "" --TODO: https

instance MonadRoute m => MonadRoute (ReaderT r m) where
  routeToUrl r = lift $ routeToUrl r
