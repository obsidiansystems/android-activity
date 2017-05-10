{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PhonePush.IOS.APNSWorker where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Pool
import Database.Groundhog.Postgresql
import Focus.Backend.DB
import Focus.Backend.DB.Groundhog
import Focus.Backend.QueueWorker (worker)
import Focus.Backend.Schema.TH
import Focus.Concurrent
import Focus.Schema

import PhonePush.IOS

instance HasId ApplePushMessage

mkFocusPersist (Just "migrateApplePushMessage") [groundhog|
  - entity: ApplePushMessage
|]

makeDefaultKeyIdInt64 ''ApplePushMessage 'ApplePushMessageKey

apnsWorker
  :: (MonadIO m, RunDb f)
  => APNSConfig
  -> Int
  -> f (Pool Postgresql)
  -> m (IO ())
apnsWorker cfg delay db = return . killThread <=<
  liftIO . forkIO . supervise . void . forever . withAPNSSocket cfg $ \conn -> do
    _ <- worker delay db $ do
      let clear = do
            qm <- Map.toList <$> selectMap ApplePushMessageConstructor (CondEmpty `limitTo` 1)
            case qm of
              [(k, m)] -> do
                liftIO $ sendApplePushMessage m conn
                deleteBy $ fromId k
                clear
              _ -> return ()
      clear
    return ()
