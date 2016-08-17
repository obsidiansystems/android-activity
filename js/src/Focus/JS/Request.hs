{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, GADTs, ScopedTypeVariables, FunctionalDependencies, RecursiveDo, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, EmptyDataDecls, NoMonomorphismRestriction, TemplateHaskell, PolyKinds, TypeOperators, DeriveFunctor, LambdaCase, CPP, ForeignFunctionInterface, JavaScriptFFI, DeriveDataTypeable, ConstraintKinds, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Focus.JS.Request where

import Data.Bitraversable
import Data.Monoid
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Data.Int
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Control
import Foreign.JavaScript.TH
import Focus.AppendMap (AppendMap)
import Focus.JS.Env
import Focus.Request
import Control.Arrow
import Control.Monad.Exception
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State

import Reflex
import Reflex.Dom hiding (Error, Value)
import Reflex.Host.Class

importJS Unsafe "console['log'](this[0])" "consoleLog" [t| forall x m. MonadJS x m => JSRef x -> m () |]

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

newtype RawXHR x = RawXHR {unRawXHR :: JSRef x}
instance ToJS x (RawXHR x) where
  withJS (RawXHR x) = ($ x)

instance FromJS x (RawXHR x) where
  fromJS = return . RawXHR

importJS Unsafe "new XMLHttpRequest()" "newXhr" [t| forall x m. MonadJS x m => m (RawXHR x) |]
importJS Unsafe "this[0]['open'](this[1], this[2], this[3])" "xhrOpen" [t| forall x m. MonadJS x m => RawXHR x -> Text -> Text -> Bool -> m () |]
importJS Unsafe "this[0]['send']()" "xhrSend" [t| forall x m. MonadJS x m => RawXHR x -> m () |]
importJS Unsafe "this[0]['send'](this[1])" "xhrSendWithData" [t| forall x m. MonadJS x m => RawXHR x -> Text -> m () |]
importJS Unsafe "this[0]['readyState']" "xhrGetReadyState" [t| forall x m. MonadJS x m => RawXHR x -> m Int |]
importJS Unsafe "this[0]['responseText']" "xhrGetResponseText" [t| forall x m. MonadJS x m => RawXHR x -> m Text |]
importJS Unsafe "this[0]['response']" "xhrGetResponse" [t| forall x m. MonadJS x m => RawXHR x -> m (JSRef x) |]

xhrSetOnReadyStateChange :: MonadJS x m => RawXHR x -> JSFun x -> m ()
xhrSetOnReadyStateChange xhr f = withJS xhr $ \x -> withJS f $ \f' -> setJSProp "onreadystatechange" f' x

xhrSetResponseType :: MonadJS x m => RawXHR x -> Text -> m ()
xhrSetResponseType xhr rt = withJS xhr $ \x -> withJS rt $ \s -> setJSProp "responseType" s x


mkRequestGeneric :: (MonadJS x m, MonadIO m, MonadFix m) => Maybe Text -> (RawXHR x -> m r) -> Text -> (RawXHR x -> m ()) -> Text -> (r -> IO a) -> m (RawXHR x)
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

mkBinaryRequest :: (MonadFix m, MonadJS x m, MonadIO m) => Text -> (RawXHR x -> m ()) -> Text -> (ByteString -> IO a) -> m (RawXHR x)
mkBinaryRequest = mkRequestGeneric (Just "arraybuffer") $ fromJSUint8Array <=< fromJS <=< xhrGetResponse

mkBinaryGet :: (MonadFix m, MonadJS x m, MonadIO m) => Text -> (ByteString -> IO a) -> m (RawXHR x)
mkBinaryGet = mkBinaryRequest "GET" xhrSend

mkRequest :: (MonadJS x m, MonadIO m, MonadFix m) => Text -> (RawXHR x -> m ()) -> Text -> (Text -> IO a) -> m (RawXHR x)
mkRequest = mkRequestGeneric Nothing xhrGetResponseText

mkGet :: (MonadJS x m, MonadIO m, MonadFix m) => Text -> (Text -> IO a) -> m (RawXHR x)
mkGet = mkRequest "GET" xhrSend

mkPost :: (MonadJS x m, MonadIO m, MonadFix m) => Text -> Text -> (Text -> IO a) -> m (RawXHR x)
mkPost url d = mkRequest "POST" (flip xhrSendWithData d) url

syncApi :: (Request r, ToJSON a, FromJSON a, MonadJS x m, MonadIO m, MonadFix m) => r a -> m a
syncApi req = do
  v <- liftIO $ newEmptyMVar
  asyncApi req $ putMVar v --TODO: Error handling
  liftIO $ takeMVar v

asyncApi :: (Request r, ToJSON a, FromJSON a, MonadJS x m, MonadIO m, MonadFix m) => r a -> (a -> IO b) -> m ()
asyncApi r f = do
  let reqJson = encode $ SomeRequest r
  _ <- mkPost "/api" (decodeUtf8 $ LBS.toStrict reqJson) $ \rspJson -> do
    Just rsp <- return $ decodeValue' $ LBS.fromStrict $ encodeUtf8 rspJson
    liftIO $ f rsp
  return ()

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

importJS Unsafe "decodeURIComponent(window['location']['search'])" "getWindowLocationSearch" [t| forall x m. MonadJS x m => m Text |]

data RequestEnv t m = RequestEnv { _requestEnv_response :: Event t ((Int64, Int64), Either Text Value)
                                 , _requestEnv_currentInvocation :: Int64
                                 , _requestEnv_nextInvocation :: Ref (WidgetHost m) Int64
                                 }

type RequestOutput t req = Event t [((Int64, Int64), SomeRequest req)]

minId :: Int64
minId = 1

newtype RequestT t req m a = 
  RequestT { unRequestT :: StateT Int64 (ReaderT (RequestEnv t m) (DynamicWriterT t (RequestOutput t req) m)) a }
 deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t, MonadSample t, MonadAsyncException, MonadException)

instance MonadReader r m => MonadReader r (RequestT t req m) where
  ask = lift ask
  local f (RequestT a) = RequestT $ mapStateT (mapReaderT $ local f) a
  reader = lift . reader

runRequestT :: (Reflex t, Monad m, MonadFix m, MonadHold t m, Ref (Performable m) ~ Ref IO, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadIO m, MonadIO (Performable m), HasWebView m, HasJS x m, HasJS x (WidgetHost m), FromJSON notification, ToJSON token, ToJSON vs, FromJSON token, Ord token, Request req)
            => Event t (AppendMap token vs)
            -> RequestT t req m a
            -> m (a, Event t (AppendMap token notification))
runRequestT eViewSelectorWithAuth (RequestT m) = do
  nextInvocation <- liftIO $ newRef (minId + 1)
  rec (eNotification, eResponse) <- openAndListenWebsocket (fmap (map (toJSON *** toJSON)) $ switchPromptlyDyn dReq) eViewSelectorWithAuth
      let rEnv = RequestEnv { _requestEnv_response = fmapMaybe (bisequence . (valueToMaybe *** Just)) eResponse
                            , _requestEnv_currentInvocation = minId
                            , _requestEnv_nextInvocation = nextInvocation
                            }
      ((a, _), dReq) <- runDynamicWriterT (runReaderT (runStateT m minId) rEnv)
  return (a, eNotification)
  where
    valueToMaybe r = case fromJSON r of
      Error _ -> error $ "runRequestTWebSocket failed to parse " <> show r
      Success a -> Just a

withRequestT :: forall t m a (req :: * -> *) (req' :: * -> *).
                (Reflex t, MonadHold t m, MonadFix m)
             => (forall x. req x -> req' x)
             -> RequestT t req m a
             -> RequestT t req' m a
withRequestT f m = RequestT $ hoist (hoist (withDynamicWriterT f')) (unRequestT m)
  where
    f' :: Event t [(y, SomeRequest req)] -> Event t [(y, SomeRequest req')]
    f' = fmap . fmap . fmap $ \(SomeRequest req) -> SomeRequest $ f req

withDynamicWriterT :: (Monoid w, Reflex t, MonadHold t m, MonadFix m)
                   => (w -> w')
                   -> DynamicWriterT t w m a
                   -> DynamicWriterT t w' m a
withDynamicWriterT f dw = do
  (r, d) <- lift $ do
    (r, d) <- runDynamicWriterT dw
    let d' = fmap f d
    return (r, d')
  tellDyn d
  return r

instance MonadTrans (RequestT t req) where
  lift = RequestT . lift . lift . lift

instance MonadRef m => MonadRef (RequestT t req m) where
  type Ref (RequestT t req m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance HasWebView m => HasWebView (RequestT t req m) where
  type WebViewPhantom (RequestT t req m) = WebViewPhantom m
  askWebView = lift askWebView

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (RequestT t req m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

class Monad m => MonadRequest t req m | m -> t where
  requesting :: (ToJSON a, FromJSON a) => Event t (req a) -> m (Event t a)
  -- requestingMany :: (ToJSON a, FromJSON a, Traversable f, HasJS x (WidgetHost m)) => Event t (f (req a)) -> m (Event t (f a))

requesting_ :: (ToJSON a, FromJSON a, MonadRequest t req m) => Event t (req a) -> m ()
requesting_ a = requesting a >> return ()

{-
liftRequestTThroughSync :: Monad m => (m (a, Int64) -> DynamicWriterT t (RequestOutput t req) m (a, Int64)) -> _ -> RequestT t req m a -> RequestT t req m a
liftRequestTThroughSync f g a = RequestT $ do
    s <- get
    env <- ask
    (a, newS) <- lift $ lift $ f $ runReaderT (runStateT (g a) s) env
    put newS
    return a
-}
liftRequestTAsync :: (Ref (Performable m) ~ Ref m, MonadAtomicRef m) => ((forall b. RequestT t req m b -> DynamicWriterT t (RequestOutput t req) m b) -> DynamicWriterT t (RequestOutput t req) m a) -> RequestT t req m a
liftRequestTAsync a = RequestT $ do
  env <- ask
  lift $ lift $ a $ \(RequestT x) -> do
    s <- atomicModifyRef (_requestEnv_nextInvocation env) $ \s -> (succ s, s)
    runReaderT (evalStateT x minId) $ env { _requestEnv_currentInvocation = s }

--TODO: Synchronous versions of some of these things, so we can avoid screwing around with the IORef so much
instance (DomBuilder t m, Ref (Performable m) ~ Ref m, MonadAtomicRef m, MonadFix m, MonadHold t m) => DomBuilder t (RequestT t req m) where
  type DomBuilderSpace (RequestT t req m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg child = liftRequestTAsync $ \run -> element elementTag (fmap1 run cfg) $ run child
  placeholder cfg = liftRequestTAsync $ \run -> placeholder $ fmap1 run cfg
  inputElement cfg = liftRequestTAsync $ \run -> inputElement $ fmap1 run cfg
  textAreaElement cfg = liftRequestTAsync $ \run -> textAreaElement $ fmap1 run cfg
  selectElement cfg child = liftRequestTAsync $ \run -> selectElement (fmap1 run cfg) $ run child
  placeRawElement = lift . placeRawElement
  wrapRawElement e cfg = liftRequestTAsync $ \run -> wrapRawElement e $ fmap1 run cfg

instance (Deletable t m, MonadHold t m, MonadFix m) => Deletable t (RequestT t req m) where
  deletable delete = RequestT . liftThrough (liftThrough (deletable delete)) . unRequestT

instance PerformEvent t m => PerformEvent t (RequestT t req m) where
  type Performable (RequestT t req m) = Performable m --TODO: Should we support 'requesting' in the Performable?
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance PostBuild t m => PostBuild t (RequestT t req m) where
  getPostBuild = lift getPostBuild

instance TriggerEvent t m => TriggerEvent t (RequestT t req m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance (MonadFix (WidgetHost m), DomBuilder t m, Request req) => MonadRequest t req (RequestT t req m) where
  requesting e = do
    rid <- RequestT $ state $ \s -> (s, s + 1)
    c <- RequestT $ asks _requestEnv_currentInvocation
    RequestT $ lift $ lift $ tellDyn $ constDyn $ fmap ((:[]) . (,) (rid, c) . SomeRequest) e
    rsp <- RequestT $ asks _requestEnv_response
    return $ fforMaybe rsp $ \(rspId, r) ->
      if rspId == (rid, c)
         then case r of
                   Left _ -> Nothing
                   Right a -> case fromJSON a of
                                   Error _ -> error $ "MonadRequest: Parse failed: " <> show r
                                   Success a' -> Just a'
         else Nothing

instance MonadRequest t req m => MonadRequest t req (ReaderT r m) where
  requesting = lift . requesting

-- TODO move these to reflex
instance MonadSample t m => MonadSample t (StateT s m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (StateT s m) where
  hold a = lift . hold a
  holdDyn a = lift . holdDyn a
  holdIncremental a = lift . holdIncremental a
