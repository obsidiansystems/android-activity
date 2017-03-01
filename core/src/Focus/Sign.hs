{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable, DefaultSignatures, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module Focus.Sign where

import Data.Text
import Data.Typeable
import Data.Aeson
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as Strict

newtype Signed a = Signed { unSigned :: Text } deriving (Show, Read, Eq, Ord, ToJSON, FromJSON)

class Monad m => MonadSign m where
  sign :: (Typeable a, ToJSON a) => a -> m (Signed a) -- We need the Typeable here because otherwise two Signeds whose contents encode the same way will be interchangeable
  readSigned :: (Typeable a, FromJSON a) => Signed a -> m (Maybe a)

instance MonadSign m => MonadSign (ReaderT r m) where
  sign = lift . sign
  readSigned = lift . readSigned

instance MonadSign m => MonadSign (StateT s m) where
  sign = lift . sign
  readSigned = lift . readSigned

instance MonadSign m => MonadSign (Strict.StateT s m) where
  sign = lift . sign
  readSigned = lift . readSigned
