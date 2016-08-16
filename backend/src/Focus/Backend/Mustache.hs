{-# LANGUAGE TemplateHaskell, FlexibleInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, UndecidableInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Focus.Backend.Mustache
( module Focus.Backend.Mustache
, (~>)
, substituteValue
) where

import Data.Vector (fromList)

import Focus.Brand
import Focus.Sign
import Focus.Route
import Focus.Email
import Focus.Backend.TH
import Control.Monad.IO.Class
import Control.Monad.Reader

import Text.Mustache
import Text.Mustache.Types
import Text.Parsec.Error

class MonadIO m => MonadMustache m where
  compileMustacheWithError :: FilePath -> m (Either ParseError Template)

compileMustache :: MonadMustache m => FilePath -> m (Maybe Template)
compileMustache template = toMaybe <$> compileMustacheWithError template
  where
    toMaybe (Left _) = Nothing
    toMaybe (Right x) = Just x

type MustachePaths = [FilePath]
newtype MustacheT m a = MustacheT { unMustacheT :: ReaderT MustachePaths m a } deriving (Functor, Applicative, Monad, MonadIO, MonadRoute r, MonadSign, MonadBrand, MonadEmail, MonadTrans)

instance MonadIO m => MonadMustache (MustacheT m) where
  compileMustacheWithError template = do
    paths <- MustacheT ask
    liftIO $ automaticCompile paths template

instance MonadMustache m => MonadMustache (ReaderT r m) where
  compileMustacheWithError = lift . compileMustacheWithError

runMustacheT :: MustacheT m a -> MustachePaths -> m a
runMustacheT = runReaderT . unMustacheT

deriveNewtypePersistBackend (\m -> [t| MustacheT $m |]) (\m -> [t| ReaderT MustachePaths $m |]) 'MustacheT 'unMustacheT

-- Util
mustacheObject :: [Pair] -> Value
mustacheObject = object

mustacheArray :: [Value] -> Value
mustacheArray = Array . fromList
