{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Focus.Backend.EmailWorker where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH
import Data.Maybe
import Data.Pool
import Data.Time (UTCTime)
import Database.Groundhog.Postgresql hiding (Cond)
import Database.Groundhog.TH
import qualified Network.Mail.Mime as Mail

import qualified Focus.AppendMap as Map
import Focus.Backend.DB
import Focus.Backend.DB.Groundhog
import Focus.Backend.DB.PsqlSimple
import Focus.Backend.Email
import Focus.Backend.Schema ()
import Focus.Backend.Schema.TH
import Focus.Schema

-- | Emails waiting to be sent
data QueuedEmail = QueuedEmail { _queuedEmail_mail :: Json Mail.Mail
                               , _queuedEmail_expiry :: Maybe UTCTime
                               }

instance HasId QueuedEmail

mkFocusPersist (Just "migrateQueuedEmail") [groundhog|
  - entity: QueuedEmail
|]
makeDefaultKeyIdInt64 ''QueuedEmail 'QueuedEmailKey

-- | Queues a single email
queueMail :: PersistBackend m
          => Mail.Mail
          -> Maybe UTCTime
          -> m (Id QueuedEmail)
queueMail m t = fmap toId . insert $ QueuedEmail { _queuedEmail_mail = Json m
                                                 , _queuedEmail_expiry = t
                                                 }

-- Retrieves and sends emails one at a time, deleting them from the queue
clearMailQueue :: (MonadIO m, PersistBackend m, PostgresRaw m)
               => EmailEnv
               -> m ()
clearMailQueue emailEnv = do
  queuedEmail <- listToMaybe . Map.toList <$> selectMap' QueuedEmailConstructor (CondEmpty `limitTo` 1)
  case queuedEmail of
    Nothing -> return ()
    Just (qid, QueuedEmail (Json email) expiry) -> do
      notExpired <- case expiry of
        Nothing -> return True
        Just expiry' -> do
          now <- getTime
          return $ now < expiry'
      when notExpired (runEmailT (sendMail email) emailEnv)
      _ <- execute [sql| DELETE FROM "QueuedEmail" q WHERE q.id = ? |] (Only qid)
      clearMailQueue emailEnv

-- | Spawns a thread to monitor mail queue table and send emails if necessary
emailWorker :: (MonadIO m, RunDb f)
            => Int -- ^ Thread delay
            -> f (Pool Postgresql)
            -> EmailEnv
            -> m (IO ()) -- ^ Action that kills the email worker thread
emailWorker delay db emailEnv = return . killThread <=< liftIO . forkIO . forever $
  handle (\(e :: SomeException) -> print e) $ runDb db (clearMailQueue emailEnv) >> threadDelay delay

deriveJSON defaultOptions ''Mail.Address
deriveJSON defaultOptions ''Mail.Encoding
deriveJSON defaultOptions ''Mail.Part
deriveJSON defaultOptions ''Mail.Mail
