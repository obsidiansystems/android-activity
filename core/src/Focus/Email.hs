{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable, DefaultSignatures, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module Focus.Email where

import Control.Monad.Reader
import Network.Mail.Mime (Mail)

class Monad m => MonadEmail m where
  sendMail :: Mail -> m ()

instance MonadEmail m => MonadEmail (ReaderT r m) where
  sendMail = lift . sendMail
