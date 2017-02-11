{-# LANGUAGE CPP, FlexibleInstances, UndecidableInstances, TemplateHaskell, GeneralizedNewtypeDeriving, GADTs #-}
module Focus.Brand where

import Focus.Request
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader
#if !defined(USE_TEMPLATE_HASKELL) || defined(DUMPING_SPLICES)
import Data.Aeson
import Focus.Request (HList(..))
#endif

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

newtype BrandT m a = BrandT { unBrandT :: ReaderT Brand m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadTrans)

instance Monad m => MonadBrand (BrandT m) where
  getBrand = BrandT ask

instance MonadBrand m => MonadBrand (ReaderT r m) where
  getBrand = lift getBrand

runBrandT :: BrandT m a -> Brand -> m a
runBrandT = runReaderT . unBrandT

#ifdef USE_TEMPLATE_HASKELL
makeJson ''Brand
#else
instance ToJSON Brand where
  toJSON r_a2IKq
    = case r_a2IKq of {
        Brand f_a2IKs f_a2IKt f_a2IKu f_a2IKv
          -> toJSON
               ("Brand" :: String,
              toJSON
                (HCons
                   f_a2IKs (HCons f_a2IKt (HCons f_a2IKu (HCons f_a2IKv HNil))))) }
instance FromJSON Brand where
  parseJSON v_a2IKy
    = do { (tag'_a2IKz, v'_a2IKA) <- parseJSON v_a2IKy;
           case tag'_a2IKz :: String of
             "Brand"
               -> do { HCons f_a2IKC
                             (HCons f_a2IKD (HCons f_a2IKE (HCons f_a2IKF HNil))) <- parseJSON
                                                                                       v'_a2IKA;
                       return (Brand f_a2IKC f_a2IKD f_a2IKE f_a2IKF) }
             _ -> fail "invalid message" }
#endif
