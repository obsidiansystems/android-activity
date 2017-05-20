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
import qualified Data.Map as Map
import Data.Pool
import Database.Groundhog.Postgresql
import Focus.Backend.DB
import Focus.Backend.DB.Groundhog
import Focus.Backend.QueueWorker
import Focus.Backend.Schema.TH
import Focus.Concurrent
import Focus.Request
import Focus.Schema

import Network.HTTP.Conduit

import PhonePush.Android
import PhonePush.Android.Payload
import PhonePush.IOS


instance HasId ApplePushMessage

mkFocusPersist (Just "migrateApplePushMessage") [groundhog|
  - entity: ApplePushMessage
|]

makeDefaultKeyIdInt64 ''ApplePushMessage 'ApplePushMessageKey

data AndroidPushMessage = AndroidPushMessage
  { _androidPushMessage_payload :: Json FcmPayload }

instance HasId AndroidPushMessage

makeJson ''AndroidPushMessage

mkFocusPersist (Just "migrateAndroidPushMessage") [groundhog|
  - entity: AndroidPushMessage
|]
makeDefaultKeyIdInt64 ''AndroidPushMessage 'AndroidPushMessageKey

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
              selectMap ApplePushMessageConstructor (CondEmpty `limitTo` 1)
            case qm of
              [(k, m)] -> do
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
          p <- runDb db $ Map.toList <$>
            selectMap AndroidPushMessageConstructor (CondEmpty `limitTo` 1)
          case p of
            [(k, AndroidPushMessage (Json m))] -> do
              sendAndroidPushMessage mgr key m
              runDb db $ deleteBy $ fromId k
              clear
            _ -> return ()
    clear
