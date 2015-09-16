{-# LANGUAGE FlexibleInstances, UndecidableInstances, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Focus.Brand where

import Focus.Request
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Control.Applicative
import Control.Monad.Reader

data Brand
  = Brand { _brand_productName :: Text
          , _brand_description :: Text
          , _brand_defaultEmailName :: Text
          , _brand_defaultEmail :: Text
          }

instance Show Brand where
  show = T.unpack . _brand_productName

class Monad m => MonadBrand m where
  getBrand :: m Brand

getProductName :: MonadBrand m => m String
getProductName = liftM (T.unpack . _brand_productName) getBrand

newtype BrandT m a = BrandT { unBrandT :: ReaderT Brand m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance Monad m => MonadBrand (BrandT m) where
  getBrand = BrandT ask

instance MonadBrand m => MonadBrand (ReaderT r m) where
  getBrand = lift getBrand

runBrandT :: BrandT m a -> Brand -> m a
runBrandT = runReaderT . unBrandT

makeJson ''Brand
