{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Focus.Backend.EmailWorker where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Pool
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)
import Database.Groundhog.Postgresql hiding (Cond)
import Database.Groundhog.TH
import qualified Network.Mail.Mime as Mail
import Network.Mail.SMTP

import qualified Focus.AppendMap as Map
import Focus.Backend.DB
import Focus.Backend.DB.Groundhog
import Focus.Backend.DB.PsqlSimple
import Focus.Backend.Email
import Focus.Backend.Schema ()
import Focus.Backend.Schema.TH
import Focus.Concurrent (supervise)
import Focus.Schema

-- | Emails waiting to be sent
data QueuedEmail = QueuedEmail { _queuedEmail_mail :: LargeObjectId
                               , _queuedEmail_to :: Json [Mail.Address]
                               , _queuedEmail_from :: Json Mail.Address
                               , _queuedEmail_expiry :: Maybe UTCTime
                               }

instance HasId QueuedEmail

mailToQueuedEmail :: (PostgresLargeObject m, MonadIO m) => Mail.Mail -> Maybe UTCTime -> m QueuedEmail
mailToQueuedEmail m t = do
  r <- liftIO $ Mail.renderMail' m
  (oid, _) <- newLargeObjectLBS r
  return $ QueuedEmail { _queuedEmail_mail = oid
                       , _queuedEmail_to = Json $ Mail.mailTo m
                       , _queuedEmail_from = Json $ Mail.mailFrom m
                       , _queuedEmail_expiry = t
                       }

mkFocusPersist (Just "migrateQueuedEmail") [groundhog|
  - entity: QueuedEmail
|]

makeDefaultKeyIdInt64 ''QueuedEmail 'QueuedEmailKey

-- | Queues a single email
queueMail :: (PostgresLargeObject m, PersistBackend m, MonadIO m)
          => Mail.Mail
          -> Maybe UTCTime
          -> m (Id QueuedEmail)
queueMail m t = do
  qm <- mailToQueuedEmail m t
  fmap toId . insert $ qm

-- Retrieves and sends emails one at a time, deleting them from the queue
clearMailQueue :: (MonadIO m, PersistBackend m, PostgresLargeObject m)
               => EmailEnv
               -> m ()
clearMailQueue emailEnv = do
  queuedEmail <- listToMaybe . Map.toList <$> selectMap' QueuedEmailConstructor (CondEmpty `limitTo` 1)
  case queuedEmail of
    Nothing -> return ()
    Just (qid, QueuedEmail oid (Json to) (Json from) expiry) -> do
      notExpired <- case expiry of
        Nothing -> return True
        Just expiry' -> do
          now <- getTime
          return $ now < expiry'
      when notExpired $ do
        withStreamedLargeObject oid $ \payload -> sendQueuedEmail emailEnv from to (LBS.toStrict payload)
        liftIO $ putStrLn $ "Sending email to: " ++ show to
      _ <- execute [sql| DELETE FROM "QueuedEmail" q WHERE q.id = ? |] (Only qid)
      deleteLargeObject oid
      clearMailQueue emailEnv

sendQueuedEmail :: EmailEnv -> Mail.Address -> [Mail.Address] -> ByteString -> IO ()
sendQueuedEmail (host, port, user, pass) sender recipients payload = do
  let from = T.encodeUtf8 . Mail.addressEmail $ sender
      to = map (T.encodeUtf8 . Mail.addressEmail) recipients
  conn <- connectSMTP' host port
  _ <- sendCommand conn (AUTH LOGIN user pass)
  sendRenderedMail from to payload conn
  closeSMTP conn

-- | Spawns a thread to monitor mail queue table and send emails if necessary
emailWorker :: (MonadIO m, RunDb f)
            => Int -- ^ Thread delay
            -> f (Pool Postgresql)
            -> EmailEnv
            -> m (IO ()) -- ^ Action that kills the email worker thread
emailWorker delay db emailEnv = return . killThread <=< liftIO . forkIO . supervise .  void . forever $
  runDb db (clearMailQueue emailEnv) >> threadDelay delay

deriveJSON defaultOptions ''Mail.Address
deriveJSON defaultOptions ''Mail.Encoding
deriveJSON defaultOptions ''Mail.Part
deriveJSON defaultOptions ''Mail.Mail