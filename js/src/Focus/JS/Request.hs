{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, GADTs, ScopedTypeVariables, FunctionalDependencies, RecursiveDo, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, EmptyDataDecls, NoMonomorphismRestriction, TemplateHaskell, PolyKinds, TypeOperators, DeriveFunctor, LambdaCase, CPP, ForeignFunctionInterface, JavaScriptFFI, DeriveDataTypeable, ConstraintKinds #-}
module Focus.JS.Request where

import Data.Monoid
import Data.Time.Clock
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Foreign.JavaScript.TH
import Focus.Request
import Control.Monad.IO.Class
import Control.Monad.Fix
import Reflex
import Reflex.Dom.Class

importJS Unsafe "console['log'](this[0])" "consoleLog" [t| forall x m. MonadJS x m => JSRef x -> m () |]

validJSRef :: MonadJS x m => JSRef x -> m (Maybe (JSRef x))
validJSRef r = do
  u <- isJSUndefined r
  n <- isJSNull r
  return $ if u || n then Nothing else Just r

timeFrom :: UTCTime -> UTCTime -> String
timeFrom t ct =
  let d = round $ diffUTCTime ct t
  in describe d
  where
    describeAbs :: Integer -> String
    describeAbs n
      | n >= 86400 = let days = n `Prelude.div` 86400 in show days <> if days == 1 then " day " else " days "
      | n >= 3600 = let hrs = n `Prelude.div` 3600 in show hrs <> if hrs == 1 then " hour " else " hours "
      | n >= 60 = let mins = n `Prelude.div` 60 in show mins <> if mins == 1 then " minute " else " minutes "
      | n > 0 = show n <> if n == 1 then " second " else " seconds "
      | otherwise = ""
    describe :: Integer -> String
    describe n = case n `compare` 0 of
      GT -> describeAbs n <> "ago"
      EQ -> "now"
      LT -> describeAbs (abs n) <> "from now"

newtype RawXHR x = RawXHR {unRawXHR :: JSRef x}
instance ToJS x (RawXHR x) where
  withJS (RawXHR x) = ($ x)

instance FromJS x (RawXHR x) where
  fromJS = return . RawXHR

importJS Unsafe "new XMLHttpRequest()" "newXhr" [t| forall x m. MonadJS x m => m (RawXHR x) |]
importJS Unsafe "this[0]['open'](this[1], this[2], this[3])" "xhrOpen" [t| forall x m. MonadJS x m => RawXHR x -> String -> String -> Bool -> m () |]
importJS Unsafe "this[0]['send']()" "xhrSend" [t| forall x m. MonadJS x m => RawXHR x -> m () |]
importJS Unsafe "this[0]['send'](this[1])" "xhrSendWithData" [t| forall x m. MonadJS x m => RawXHR x -> String -> m () |]
importJS Unsafe "this[0]['readyState']" "xhrGetReadyState" [t| forall x m. MonadJS x m => RawXHR x -> m Int |]
importJS Unsafe "this[0]['responseText']" "xhrGetResponseText" [t| forall x m. MonadJS x m => RawXHR x -> m String |]
importJS Unsafe "this[0]['response']" "xhrGetResponse" [t| forall x m. MonadJS x m => RawXHR x -> m (JSRef x) |]

xhrSetOnReadyStateChange :: MonadJS x m => RawXHR x -> JSFun x -> m ()
xhrSetOnReadyStateChange xhr f = withJS xhr $ \x -> withJS f $ \f' -> setJSProp "onreadystatechange" f' x

xhrSetResponseType :: MonadJS x m => RawXHR x -> String -> m ()
xhrSetResponseType xhr rt = withJS xhr $ \x -> withJSString rt $ \s -> setJSProp "responseType" s x


mkRequestGeneric :: (MonadJS x m, MonadIO m, MonadFix m) => Maybe String -> (RawXHR x -> m r) -> String -> (RawXHR x -> m ()) -> String -> (r -> IO a) -> m (RawXHR x)
mkRequestGeneric responseType convertResponse method send url cb = do
  xhr <- newXhr
  xhrOpen xhr method url True
  maybe (return ()) (xhrSetResponseType xhr) responseType
  rec callback <- mkJSFun $ \_ -> do
        readyState <- xhrGetReadyState xhr
        if readyState == 4
           then do
             r <- convertResponse xhr
             _ <- liftIO $ cb r
             freeJSFun callback
             mkJSUndefined
           else mkJSUndefined
  xhrSetOnReadyStateChange xhr callback
  _ <- send xhr
  return xhr

mkBinaryRequest :: (MonadFix m, MonadJS x m, MonadIO m) => String -> (RawXHR x -> m ()) -> String -> (ByteString -> IO a) -> m (RawXHR x)
mkBinaryRequest = mkRequestGeneric (Just "arraybuffer") $ fromJSUint8Array <=< fromJS <=< xhrGetResponse

mkBinaryGet :: (MonadFix m, MonadJS x m, MonadIO m) => String -> (ByteString -> IO a) -> m (RawXHR x)
mkBinaryGet = mkBinaryRequest "GET" xhrSend

mkRequest :: (MonadJS x m, MonadIO m, MonadFix m) => String -> (RawXHR x -> m ()) -> String -> (String -> IO a) -> m (RawXHR x)
mkRequest = mkRequestGeneric Nothing xhrGetResponseText

mkGet :: (MonadJS x m, MonadIO m, MonadFix m) => String -> (String -> IO a) -> m (RawXHR x)
mkGet = mkRequest "GET" xhrSend

mkPost :: (MonadJS x m, MonadIO m, MonadFix m) => String -> String -> (String -> IO a) -> m (RawXHR x)
mkPost url d = mkRequest "POST" (flip xhrSendWithData d) url

syncApi :: (Request r, ToJSON a, FromJSON a, MonadJS x m, MonadIO m, MonadFix m) => r a -> m a
syncApi req = do
  v <- liftIO $ newEmptyMVar
  asyncApi req $ putMVar v --TODO: Error handling
  liftIO $ takeMVar v

asyncApi :: (Request r, ToJSON a, FromJSON a, MonadJS x m, MonadIO m, MonadFix m) => r a -> (a -> IO b) -> m ()
asyncApi r f = do
  let reqJson = encode $ SomeRequest r
  _ <- mkPost "/api" (T.unpack $ decodeUtf8 $ LBS.toStrict reqJson) $ \rspJson -> do
    Just rsp <- return $ decodeValue' $ LBS.fromStrict $ encodeUtf8 $ T.pack rspJson
    liftIO $ f rsp
  return ()

requesting :: (Request r, ToJSON a, FromJSON a, MonadWidget t m) => Event t (r a) -> m (Event t a)
requesting requestE = performEventAsync $ fmap (\r yield -> liftIO $ asyncApi r yield) requestE

requestingMany :: (Request r, ToJSON a, FromJSON a, MonadWidget t m, Traversable f) => Event t (f (r a)) -> m (Event t (f a))
requestingMany requestsE = performEventAsync $ ffor requestsE $ \rs cb -> do
  resps <- forM rs $ \r -> do
    resp <- liftIO newEmptyMVar
    _ <- liftIO (asyncApi r $ liftIO . putMVar resp)
    return resp
  _ <- liftIO . forkIO $ cb =<< forM resps takeMVar
  return ()

importJS Unsafe "decodeURIComponent(window['location']['search'])" "getWindowLocationSearch" [t| forall x m. MonadJS x m => m String |]

{-
mkRequest :: (MonadFix m, MonadJS x m, MonadIO m) => String -> (RawXHR x -> IO ()) -> String -> (String -> IO a) -> m (RawXHR x)
(MonadFix m, MonadJS x m, MonadIO m)
mkGet :: (MonadFix m, MonadJS x m, MonadIO m) => String -> (String -> IO a) -> m (RawXHR x)

mkPost :: (MonadFix m, MonadJS x m, MonadIO m) => String -> String -> (String -> IO a) -> m (RawXHR x)

mkBinaryRequest ::(MonadFix m, MonadJS x m, MonadIO m) =>  String -> (RawXHR x -> IO ()) -> String -> (ByteString -> IO a) -> m (RawXHR x)

mkBinaryGet :: (MonadFix m, MonadJS x m, MonadIO m) => String -> (ByteString -> IO a) -> m (RawXHR x)


-}
