{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PhonePush.Worker where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Pool
import Database.Groundhog.Postgresql
import Focus.Backend.DB
import Focus.Backend.DB.Groundhog
import Focus.Backend.QueueWorker
import Focus.Backend.Schema.TH
import Focus.Concurrent
import Focus.Schema

import Network.HTTP.Conduit

import PhonePush.Android
import PhonePush.Android.Payload
import PhonePush.IOS


data QueuedApplePushMessage = QueuedApplePushMessage
  { _queuedApplePushMessage_applePushMessage :: ApplePushMessage
  , _queuedApplePushMessage_checkedOut :: Bool
  }
instance HasId QueuedApplePushMessage

mkFocusPersist (Just "migrateApplePushMessage") [groundhog|
  - entity: QueuedApplePushMessage
    constructors:
      - name: QueuedApplePushMessage
        fields:
          - name: _queuedApplePushMessage_checkedOut
            default: "false"
  - embedded: ApplePushMessage
|]
makeDefaultKeyIdInt64 ''QueuedApplePushMessage 'QueuedApplePushMessageKey

data QueuedAndroidPushMessage = QueuedAndroidPushMessage
  { _queuedAndroidPushMessage_payload :: Json FcmPayload
  , _queuedAndroidPushMessage_checkedOut :: Bool
  }
instance HasId QueuedAndroidPushMessage

mkFocusPersist (Just "migrateAndroidPushMessage") [groundhog|
  - entity: QueuedAndroidPushMessage
    constructors:
      - name: QueuedAndroidPushMessage
        fields:
          - name: _queuedAndroidPushMessage_checkedOut
            default: "false"
|]
makeDefaultKeyIdInt64 ''QueuedAndroidPushMessage 'QueuedAndroidPushMessageKey

apnsWorker
  :: (MonadIO m, RunDb f)
  => APNSConfig
  -> Int
  -> f (Pool Postgresql)
  -> m (IO ())
apnsWorker cfg delay db = return . killThread <=<
  liftIO . forkIO . supervise . withAPNSSocket cfg $ \conn -> do
    void $ forever $ do
      let clear = do
            qm <- Map.toList <$>
              selectMap QueuedApplePushMessageConstructor
                        ((QueuedApplePushMessage_checkedOutField ==. False) `limitTo` 1)
            case qm of
              [(k, QueuedApplePushMessage m _)] -> do
                update [QueuedApplePushMessage_checkedOutField =. True] $ AutoKeyField ==. fromId k
                if LBS.length (_applePushMessage_payload m) > maxPayloadLength
                  then deleteBy (fromId k) >> clear
                  else do
                    liftIO $ sendApplePushMessage conn m
                    deleteBy $ fromId k
                    clear
              _ -> return ()
      runDb db clear >> threadDelay delay
    return ()

firebaseWorker
  :: (MonadIO m, RunDb f)
  => ByteString -- ^ Firebase server key
  -> Int -- ^ Sleep length when queue is clear (seconds)
  -> f (Pool Postgresql) -- ^ DB pool
  -> m (IO ()) -- ^ IO Action to kill thread
firebaseWorker key delay db = do
  mgr <- liftIO $ newManager tlsManagerSettings
  worker delay $ do
    let clear = do
          p <- Map.toList <$>
            selectMap QueuedAndroidPushMessageConstructor
                      ((QueuedAndroidPushMessage_checkedOutField ==. False) `limitTo` 1)
          case p of
            [(k, QueuedAndroidPushMessage (Json m) _)] -> do
              update [QueuedAndroidPushMessage_checkedOutField =. True] $ AutoKeyField ==. fromId k
              liftIO $ sendAndroidPushMessage mgr key m
              deleteBy $ fromId k
              clear
            _ -> return ()
    runDb db clear
