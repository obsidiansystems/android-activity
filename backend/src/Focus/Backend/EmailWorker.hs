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

import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Groundhog.Postgresql hiding (Cond)
import Database.Groundhog.TH
import qualified Network.Mail.Mime as Mail
import qualified Network.HaskellNet.SMTP as SMTP

import Focus.Aeson.Orphans ()
import qualified Focus.AppendMap as Map
import Focus.Backend.DB
import Focus.Backend.DB.Groundhog
import Focus.Backend.DB.PsqlSimple
import Focus.Backend.Email
import Focus.Backend.QueueWorker
import Focus.Backend.Schema ()
import Focus.Backend.Schema.TH
import Focus.Schema

-- | Emails waiting to be sent
data QueuedEmail = QueuedEmail { _queuedEmail_mail :: LargeObjectId
                               , _queuedEmail_to :: Json [Mail.Address]
                               , _queuedEmail_from :: Json Mail.Address
                               , _queuedEmail_expiry :: Maybe UTCTime
                               , _queuedEmail_checkedOut :: Bool
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
                       , _queuedEmail_checkedOut = False
                       }

mkFocusPersist (Just "migrateQueuedEmail") [groundhog|
  - entity: QueuedEmail
    constructors:
      - name: QueuedEmail
        fields:
          - name: _queuedEmail_checkedOut
            default: "false"
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
clearMailQueue :: forall f. RunDb f
               => f (Pool Postgresql)
               -> EmailEnv
               -> IO ()
clearMailQueue db emailEnv = do
  queuedEmail <- (runDb db :: FocusPersist a -> IO a) $ do
    qe <- listToMaybe . Map.toList <$>
      selectMap' QueuedEmailConstructor ((QueuedEmail_checkedOutField ==. False) `limitTo` 1)
    forM_ (fst <$> qe) $ \eid ->
      update [QueuedEmail_checkedOutField =. True] $ AutoKeyField ==. fromId eid
    return qe
  case queuedEmail of
    Nothing -> return ()
    Just (qid, QueuedEmail oid (Json to) (Json from) expiry _) -> do
      runDb db $ do
        now <- getTime
        notExpired <- case expiry of
          Nothing -> return True
          Just expiry' -> do
            return $ now < expiry'
        when notExpired $ do
          withStreamedLargeObject oid $ \payload -> do
            ersp <- sendQueuedEmail emailEnv from to (LBS.toStrict payload)
            case ersp of
              Left err -> liftIO $ error $ mconcat
                [ "["
                , show now
                , "] Failed to send email to "
                , show to
                , ". Error: "
                , T.unpack err
                ]
              Right _ -> liftIO $ putStrLn $ mconcat
                [ "["
                , show now
                , "] "
                , "Sending email to: "
                , show to
                ]
      runDb db $ do
        _ <- execute [sql| DELETE FROM "QueuedEmail" q WHERE q.id = ? |] (Only qid)
        deleteLargeObject oid
      clearMailQueue db emailEnv

sendQueuedEmail :: EmailEnv -> Mail.Address -> [Mail.Address] -> ByteString -> IO (Either Text ())
sendQueuedEmail env sender recipients payload = do
  let from = T.unpack . Mail.addressEmail $ sender
      to = map (T.unpack . Mail.addressEmail) recipients
  withSMTP env $ SMTP.sendMail from to payload 

-- | Spawns a thread to monitor mail queue table and send emails if necessary
emailWorker :: (MonadIO m, RunDb f)
            => Int -- ^ Thread delay
            -> f (Pool Postgresql)
            -> EmailEnv
            -> m (IO ()) -- ^ Action that kills the email worker thread
emailWorker delay db emailEnv = worker delay $ clearMailQueue db emailEnv

deriveJSON defaultOptions ''Mail.Address
deriveJSON defaultOptions ''Mail.Encoding
deriveJSON defaultOptions ''Mail.Part
deriveJSON defaultOptions ''Mail.Mail
