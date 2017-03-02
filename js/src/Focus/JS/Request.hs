{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, GADTs, ScopedTypeVariables, FunctionalDependencies, RecursiveDo, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, EmptyDataDecls, NoMonomorphismRestriction, TemplateHaskell, PolyKinds, TypeOperators, DeriveFunctor, LambdaCase, CPP, ForeignFunctionInterface, JavaScriptFFI, DeriveDataTypeable, ConstraintKinds, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Focus.JS.Request where

import Data.Monoid
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Control.Monad
import Foreign.JavaScript.TH
import Focus.Request
import Control.Monad.Reader
#ifdef __GHCJS__
import GHCJS.Marshal.Pure
import Focus.JS.WebSocket (rawDecode)
#endif
import GHCJS.DOM.Types
       (JSM, MonadJSM, XMLHttpRequest, liftJSM, PFromJSVal(..),
        unGObject, FormData)
import GHCJS.DOM.Enums (XMLHttpRequestResponseType(..))
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.XMLHttpRequest
       (newXMLHttpRequest, open, send, setResponseType,
        getReadyState, getResponseUnchecked, getResponseTextUnchecked,
        readyStateChange, sendFormData)

import qualified JavaScript.TypedArray.ArrayBuffer as JS
       (unsafeFreeze, MutableArrayBuffer)
import qualified GHCJS.Buffer as JS
       (toByteString, createFromArrayBuffer)
import Language.Javascript.JSaddle.Types (ghcjsPure)

validJSRef :: MonadJS x m => JSRef x -> m (Maybe (JSRef x))
validJSRef r = do
  u <- isJSUndefined r
  n <- isJSNull r
  return $ if u || n then Nothing else Just r

timeFrom :: UTCTime -> UTCTime -> Text
timeFrom t ct =
  let d = round $ diffUTCTime ct t
  in describe d
  where
    describeAbs :: Integer -> Text
    describeAbs n
      | n >= 86400 = let days = n `Prelude.div` 86400 in T.pack (show days) <> if days == 1 then " day " else " days "
      | n >= 3600 = let hrs = n `Prelude.div` 3600 in T.pack (show hrs) <> if hrs == 1 then " hour " else " hours "
      | n >= 60 = let mins = n `Prelude.div` 60 in T.pack (show mins) <> if mins == 1 then " minute " else " minutes "
      | n > 0 = T.pack (show n) <> if n == 1 then " second " else " seconds "
      | otherwise = ""
    describe :: Integer -> Text
    describe n = case n `compare` 0 of
      GT -> describeAbs n <> "ago"
      EQ -> "now"
      LT -> describeAbs (abs n) <> "from now"

mkRequestGeneric :: (MonadJSM m, MonadFix m)
                 => Maybe XMLHttpRequestResponseType
                 -> (XMLHttpRequest -> JSM r)
                 -> Text
                 -> (XMLHttpRequest -> m ())
                 -> Text
                 -> (r -> JSM a)
                 -> m XMLHttpRequest
mkRequestGeneric responseType convertResponse method sendFunc url cb = do
  xhr <- newXMLHttpRequest
  open xhr method url True (""::Text) (""::Text)
  maybe (return ()) (setResponseType xhr) responseType
  rec freeCallback <- liftJSM . on xhr readyStateChange . liftJSM $ do
                        readyState <- getReadyState xhr
                        when (readyState == 4) $ do
                          r <- convertResponse xhr
                          cb r
                          freeCallback
  _ <- sendFunc xhr
  return xhr

mkBinaryRequest :: (MonadFix m, MonadJSM m)
                => Text
                -> (XMLHttpRequest -> m ())
                -> Text
                -> (ByteString -> JSM a)
                -> m XMLHttpRequest
mkBinaryRequest = mkRequestGeneric (Just XMLHttpRequestResponseTypeArraybuffer) $
    bsFromArrayBuffer . pFromJSVal <=< (fmap unGObject . getResponseUnchecked)

bsFromArrayBuffer :: MonadJSM m => JS.MutableArrayBuffer -> m ByteString
bsFromArrayBuffer ab = liftJSM $ JS.unsafeFreeze ab >>=
    ghcjsPure . JS.createFromArrayBuffer >>= ghcjsPure . JS.toByteString 0 Nothing

mkBinaryGet :: (MonadFix m, MonadJSM m)
            => Text
            -> (ByteString -> JSM a)
            -> m XMLHttpRequest
mkBinaryGet = mkBinaryRequest "GET" send

mkRequest :: (MonadJSM m, MonadFix m) => Text -> (XMLHttpRequest -> m ()) -> Text -> (Text -> JSM a) -> m XMLHttpRequest
mkRequest = mkRequestGeneric Nothing getResponseTextUnchecked

mkGet :: (MonadJSM m, MonadFix m) => Text -> (Text -> JSM a) -> m XMLHttpRequest
mkGet = mkRequest "GET" send

mkPost :: (MonadJSM m, MonadFix m) => Text -> FormData -> (Text -> JSM a) -> m XMLHttpRequest
mkPost url d = mkRequest "POST" (`sendFormData` d) url

#if 0
syncApi :: (Request r, ToJSON a, FromJSON a, MonadJS x m, MonadIO m, MonadFix m) => r a -> m a
syncApi req = do
  v <- liftIO $ newEmptyMVar
  asyncApi req $ putMVar v --TODO: Error handling
  liftIO $ takeMVar v

asyncApi :: (Request r, ToJSON a, FromJSON a, MonadJS x m, MonadIO m, MonadFix m) => r a -> (a -> IO b) -> m ()
asyncApi r f = asyncApiMaybe r $ \(Just x) -> f x

asyncApiMaybe :: (Request r, ToJSON a, FromJSON a, MonadJS x m, MonadIO m, MonadFix m) => r a -> (Maybe a -> IO b) -> m ()
asyncApiMaybe r f = do
  let reqJson = encode $ SomeRequest r
  _ <- mkPost "/api" (decodeUtf8 $ LBS.toStrict reqJson) $ \rspJson -> do
#ifdef __GHCJS__
    let mrsp = rawDecode $ pToJSVal rspJson
#else
    let mrsp = decodeValue' $ LBS.fromStrict $ encodeUtf8 rspJson
#endif
    liftIO $ f mrsp
  return ()

syncApiMaybe :: (Request r, ToJSON a, FromJSON a, MonadJS x m, MonadIO m, MonadFix m) => r a -> m (Maybe a)
syncApiMaybe req = do
  v <- liftIO $ newEmptyMVar
  asyncApiMaybe req $ putMVar v
  liftIO $ takeMVar v

requestingXhr :: (Request r, ToJSON a, FromJSON a, TriggerEvent t m, PerformEvent t m, HasJS x (WidgetHost m)) => Event t (r a) -> m (Event t a)
requestingXhr requestE = performEventAsync $ fmap (\r yield' -> liftJS $ asyncApi r yield') requestE

requestingXhrMany :: (Request r, ToJSON a, FromJSON a, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m), HasJS x (WidgetHost m), Traversable f) => Event t (f (r a)) -> m (Event t (f a))
requestingXhrMany requestsE = performEventAsync $ ffor requestsE $ \rs cb -> do
  resps <- forM rs $ \r -> do
    resp <- liftIO newEmptyMVar
    _ <- liftJS $ asyncApi r $ liftIO . putMVar resp
    return resp
  _ <- liftIO . forkIO $ cb =<< forM resps takeMVar
  return ()
#endif

--importJS Unsafe "decodeURIComponent(window['location']['search'])" "getWindowLocationSearch" [t| forall x m. MonadJS x m => m Text |]

-- | Decode a JSON value from Text.  In JavaScript, this will use JSON.parse for
-- greater efficiency.
decodeValueFromText :: FromJSON a => Text -> Maybe a
#ifdef __GHCJS__
decodeValueFromText = rawDecode . pToJSVal
#else
decodeValueFromText = decodeValue' . LBS.fromStrict . encodeUtf8
#endif
