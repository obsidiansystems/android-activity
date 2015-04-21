module Backend.MonadEmail where

import Network.Mail.Mime (Mail (..))

class Monad m => MonadEmail m where
  sendMail :: Mail -> m ()


