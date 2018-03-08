{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PhonePush.Worker where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char                     (toLower)
import           Data.Maybe                    (listToMaybe)
import           System.IO                     (hPutStrLn, stderr)

import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Base64        as B64
import qualified Data.Map                      as Map
import           Data.Pool
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Database.Groundhog.Postgresql
import           Focus.Backend.DB
import           Focus.Backend.DB.Groundhog
import           Focus.Backend.QueueWorker
import           Focus.Backend.Schema.TH
import           Focus.Concurrent
import           Focus.Schema
import           GHC.Generics
import           Network.HTTP.Conduit

-- Android
import           PhonePush.Android
import           PhonePush.Android.Payload

-- iOS
import           Network.PushNotify.APN

data APNSConfig = APNSConfig
  { _APNSConfig_key         :: FilePath
  , _APNSConfig_certificate :: FilePath
  , _APNSConfig_ca          :: FilePath
  , _APNSConfig_sandbox     :: Bool
  , _APNSConfig_streams     :: Int
  , _APNSConfig_bundleName  :: Text
  } deriving Generic

instance FromJSON APNSConfig where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop 12 }

data QueuedApplePushMessage = QueuedApplePushMessage
  { _queuedApplePushMessage_apnToken   :: Text
  , _queuedApplePushMessage_jsonAps    :: Json JsonAps
  , _queuedApplePushMessage_checkedOut :: Bool
  } deriving (Show)
instance HasId QueuedApplePushMessage

mkFocusPersist (Just "migrateApplePushMessage") [groundhog|
  - entity: QueuedApplePushMessage
    constructors:
      - name: QueuedApplePushMessage
        fields:
          - name: _queuedApplePushMessage_checkedOut
            default: "false"
|]
makeDefaultKeyIdInt64 ''QueuedApplePushMessage 'QueuedApplePushMessageKey

data QueuedAndroidPushMessage = QueuedAndroidPushMessage
  { _queuedAndroidPushMessage_payload    :: Json FcmPayload
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

-- Some orphan FromJSON instances - if necessary we can fork
instance FromJSON JsonAps where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop 2 . map toLower }

instance FromJSON JsonApsMessage where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop 3 . map toLower }

instance FromJSON JsonApsAlert where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop 3 . map toLower }

sendQueuedAppleMessage
  :: ApnSession
  -> QueuedApplePushMessage
  -> IO ApnMessageResult
sendQueuedAppleMessage session (QueuedApplePushMessage token (Json aps) _) =
  sendMessage session (rawToken . B64.decodeLenient $ T.encodeUtf8 token) aps

newAPNSSession :: APNSConfig -> IO ApnSession
newAPNSSession cfg = newSession (_APNSConfig_key cfg)
                                (_APNSConfig_certificate cfg)
                                (_APNSConfig_ca cfg)
                                (_APNSConfig_sandbox cfg)
                                (_APNSConfig_streams cfg)
                                (T.encodeUtf8 $ _APNSConfig_bundleName cfg)

withAPNSSession :: APNSConfig -> (ApnSession -> IO ()) -> IO ()
withAPNSSession cfg f = newAPNSSession cfg >>= f

apnsWorker
  :: (MonadIO m, RunDb f)
  => APNSConfig
  -> Int
  -> f (Pool Postgresql)
  -> Maybe (TChan Text)
  -> m (IO ())
apnsWorker cfg delay db mTokenChan = return . killThread <=<
  liftIO . forkIO . supervise . withAPNSSession cfg $ \conn -> do
    void $ forever $ do
      let clear = do
            -- Get queued messages, mark them checked out
            queuedMsg <- runDb db $ do
              qm <- fmap (listToMaybe . Map.toList) $
                selectMap QueuedApplePushMessageConstructor $
                          (QueuedApplePushMessage_checkedOutField ==. False) `limitTo` 1
              forM_ qm $ \(k, _) ->
                update [QueuedApplePushMessage_checkedOutField =. True] $ AutoKeyField ==. fromId k
              return qm
            -- Send out messages
            case queuedMsg of
              Nothing -> return ()
              Just (k, qm@(QueuedApplePushMessage token _ _)) -> do
                sendQueuedAppleMessage conn qm >>= \case
                  ApnMessageResultFatalError code mreason ->
                    logError $ unlines $ [ "APNS error: " ++ show code
                                         , "Caused by: " ++ show qm
                                         , "Reason: " ++ maybe "unknown" T.unpack mreason
                                         ]

                  ApnMessageResultTemporaryError mcode mreason -> do
                    -- TODO Does temporary error indicate we should requeue for later?
                    -- Perhaps even a threadDelay here, then just marking checkedOut False ?
                    -- For now just deleting the task
                    logError $ unlines $ [ "APNS temporary error: " ++ maybe "too much concurrency" show mcode
                                         , "Caused by: " ++ show qm
                                         , "Reason: " ++ maybe "unknown" T.unpack mreason
                                         ]
                    runDb db $ deleteBy (fromId k)
                  ApnMessageResultTokenNoLongerValid mreason -> do
                    logError $ unlines $ [ "APNS token no longer valid"
                                         , "Caused by: " ++ show qm
                                         , "Reason: " ++ maybe "unknown" T.unpack mreason
                                         ]
                    runDb db $ delete $ QueuedApplePushMessage_apnTokenField ==. token
                    forM_ mTokenChan $ \chan -> atomically $ writeTChan chan token
                  _ -> runDb db $ deleteBy (fromId k)
                clear
      clear >> threadDelay delay

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
          queuedMsg <- runDb db $ do
            qm <- fmap (listToMaybe . Map.toList) $
              selectMap QueuedAndroidPushMessageConstructor $
                        (QueuedAndroidPushMessage_checkedOutField ==. False) `limitTo` 1
            forM_ qm $ \(k, _) ->
              update [QueuedAndroidPushMessage_checkedOutField =. True] $ AutoKeyField ==. fromId k
            return qm
          case queuedMsg of
            Nothing -> return ()
            Just (k, QueuedAndroidPushMessage (Json m) _) -> do
              _ <- sendAndroidPushMessage mgr key m
              runDb db $ deleteBy (fromId k)
              clear
    clear

logError :: String -> IO ()
logError = hPutStrLn stderr
