{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, TemplateHaskell, NoMonomorphismRestriction, EmptyDataDecls, RankNTypes, GADTs, RecursiveDo, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveDataTypeable, GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, UndecidableInstances, PolyKinds, PartialTypeSignatures, AllowAmbiguousTypes #-}

module Focus.JS.WebSocket where

import Focus.Request

import Prelude hiding (div, span, mapM, mapM_, concat, concatMap, all, sequence)
import qualified Prelude

import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Semigroup hiding (option)
import Data.Dependent.Map (DMap, GCompare (..), GOrdering (..), DSum (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.Ref
import Data.Text.Encoding

import Reflex
import Reflex.Dom
import Reflex.Host.Class --TODO

import Data.Default

import Data.IORef
import Control.Lens

import Data.Aeson (encode, decodeStrict', toJSON, parseJSON, FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parse)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Applicative
import Data.Align
import Data.These
import Data.Constraint

import Foreign.Ptr
import Foreign.JavaScript.TH

{-
#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

JS(newWebSocket_, "$r = new WebSocket($1); $r.binaryType = 'arraybuffer'; $r.onmessage = function(e){ $2(e.data); }; $r.onclose = function(e){ $3(); };", JSString -> JSFun (JSString -> IO ()) -> JSFun (IO ()) -> IO (JSRef JSWebSocket))
JS(webSocketSend_, "$1['send']($2)", JSRef JSWebSocket -> JSRef JSByteArray -> IO ()) --TODO: Use (JSRef ArrayBuffer) instead of JSString

data JSByteArray
JS(extractByteArray, "new Uint8Array($1_1.buf, $1_2, $2)", Ptr a -> Int -> IO (JSRef JSByteArray))

newWebSocket :: String -> (ByteString -> IO ()) -> IO () -> IO JSWebSocket
newWebSocket url onMessage onClose = do
  onMessageFun <- syncCallback1 AlwaysRetain True $ onMessage <=< bufferByteString 0 0
  rec onCloseFun <- syncCallback AlwaysRetain True $ do
        release onMessageFun
        release onCloseFun
        onClose
  liftM JSWebSocket $ newWebSocket_ (toJSString url) onMessageFun onCloseFun

webSocketSend :: JSWebSocket -> ByteString -> IO ()
webSocketSend ws payload = BS.useAsCString payload $ \cStr -> do
  ba <- extractByteArray cStr $ BS.length payload
  webSocketSend_ (unWebSocket ws) ba

JS(getLocationHost_, "location.host", IO JSString)

getLocationHost = liftM fromJSString getLocationHost_

JS(getLocationProtocol_, "location.protocol", IO JSString)

getLocationProtocol = liftM fromJSString getLocationProtocol_
-}

newtype JSWebSocket x = JSWebSocket { unWebSocket :: JSRef x }

instance ToJS x (JSWebSocket x) where
  withJS (JSWebSocket r) f = f r

instance FromJS x (JSWebSocket x) where
  fromJS = return . JSWebSocket

importJS Unsafe "(function(that) { var ws = new WebSocket(that[0]); ws['binaryType'] = 'arraybuffer'; ws['onmessage'] = function(e){ that[1](new Uint8Array(e.data)); }; ws['onclose'] = function(e){ that[2](); }; return ws; })(this)" "newWebSocket_" [t| forall x m. MonadJS x m => String -> JSFun x -> JSFun x -> m (JSWebSocket x) |]

importJS Unsafe "this[0]['send'](this[1])" "webSocketSend_" [t| forall x m. MonadJS x m => JSWebSocket x -> JSUint8Array x -> m () |]

newWebSocket :: (MonadJS x m, MonadFix m) => String -> (ByteString -> m ()) -> m () -> m (JSWebSocket x)
newWebSocket url onMessage onClose = do
  onMessageFun <- mkJSFun $ \[uint8array] -> do
    bs <- fromJSUint8Array $ JSUint8Array uint8array
    onMessage bs
    mkJSUndefined
  rec onCloseFun <- mkJSFun $ \[] -> do
        freeJSFun onMessageFun
        freeJSFun onCloseFun
        onClose
        mkJSUndefined
  newWebSocket_ url onMessageFun onCloseFun

webSocketSend :: MonadJS x m => JSWebSocket x -> ByteString -> m ()
webSocketSend ws bs = withJSUint8Array bs $ webSocketSend_ ws

importJS Unsafe "location['host']" "getLocationHost" [t| forall x m. MonadJS x m => m String |]

importJS Unsafe "location['protocol']" "getLocationProtocol" [t| forall x m. MonadJS x m => m String |]

data WebSocketConfig t
   = WebSocketConfig { _webSocketConfig_send :: Event t [ByteString]
                     }

instance Reflex t => Default (WebSocketConfig t) where
  def = WebSocketConfig never

data WebSocket t
   = WebSocket { _webSocket_recv :: Event t ByteString
               }
-- | Warning: Only one of these websockets may be opened on a given page in most browsers
webSocket :: forall x t m. (HasJS x m, HasJS x (WidgetHost m), MonadWidget t m) => String -> WebSocketConfig t -> m (WebSocket t)
webSocket path config = do
  pageHost <- liftJS getLocationHost
  pageProtocol <- liftJS getLocationProtocol
  let wsProtocol = case pageProtocol of
        "http:" -> "ws:"
        "https:" -> "wss:"
        "file:" -> "ws:"
      wsHost = case pageProtocol of
        "file:" -> "localhost:8000"
        _ -> pageHost
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  (eRecv, eRecvTriggerRef) <- newEventWithTriggerRef
  currentSocketRef <- liftIO $ newIORef Nothing
  --TODO: Disconnect if value no longer needed
  let onMessage m = liftIO $ do
        maybe (return ()) (\t -> postGui $ runWithActions [t :=> m]) =<< readRef eRecvTriggerRef
      start :: JSM m ()
      start = do
        ws <- newWebSocket (wsProtocol <> "//" <> wsHost <> path) onMessage $ do
          void $ forkJS $ do --TODO: Is the fork necessary, or do event handlers run in their own threads automatically?
            liftIO $ writeIORef currentSocketRef Nothing
            liftIO $ threadDelay 1000000
            start
        liftIO $ writeIORef currentSocketRef $ Just ws
        return ()
  liftJS start
  performEvent_ $ ffor (_webSocketConfig_send config) $ \payloads -> forM_ payloads $ \payload -> do
    mws <- liftIO $ readIORef currentSocketRef
    case mws of
      Nothing -> return () -- Discard --TODO: should we do something better here? probably buffer it, since we handle reconnection logic; how do we verify that the server has received things?
      Just ws -> do
        liftJS $ webSocketSend ws payload
  return $ WebSocket eRecv

makeLensesWith (lensRules & simpleLenses .~ True) ''WebSocketConfig

monoConst :: a -> a -> a
monoConst a _ = a

fromJSONViaAllArgsHave :: forall rsp req a. AllArgsHave (ComposeConstraint FromJSON rsp) req => req a -> Aeson.Value -> Aeson.Result (rsp a)
fromJSONViaAllArgsHave req rspRaw = case getArgDict req :: Dict (ComposeConstraint FromJSON rsp a) of
  Dict -> fromJSON rspRaw

apiSocket :: ( HasJS x m
             , HasJS x (WidgetHost m)
             , MonadWidget t m
             , Traversable' f
             , ToJSON' req
             , AllArgsHave (ComposeConstraint FromJSON rsp) req
             )
          => String
          -> Event t [f req]
          -> m (Event t (f rsp), Dynamic t (Map (Int, Int) (f (With' Int req), Int, Map Int Aeson.Value)))
apiSocket path batches = do
  batchesWithSerialNumbers <- zipListWithEvent (\n r -> (n, zip [(1 :: Int) ..] $ fmap numberAndSize' r)) [(1 :: Int) ..] batches
  rec ws <- webSocket path $ def & webSocketConfig_send .~ traceEvent "apiSocket: send" (fmap encodeMessages batchesWithSerialNumbers)
      state <- holdDyn mempty newState
      let change = flip push (align batchesWithSerialNumbers $ traceEvent "apiSocket: recv" $ _webSocket_recv ws) $ \update -> liftM Just $ do
            oldState <- sample $ current state
            return $ flip runState oldState $ do
              -- If we've received a new batch to send, add it to the pending transactions
              forM_ (update ^? here) $ \(n, fs) -> forM_ fs $ \(m, (r, sz)) -> do
                modify $ Map.insertWith (error "apiSocket: adding an existing serial number to the outgoing request buffer") (n, m) (r, sz, mempty :: Map Int Aeson.Value)
              case (update ^? there) of
                Nothing -> return Nothing
                Just msg -> do
                  let Just ((n, m, l), rsp) = decodeStrict' msg
                  Just (requests, sz, responses) <- use $ at (n, m)
                  let responses' = Map.insertWith (error "apiSocket: received a response with the same tags as another pending response") l rsp responses
                  case Map.size responses' `compare` sz of
                    LT -> do
                      at (n, m) .= Just (requests, sz, responses')
                      return Nothing
                    EQ -> do
                      at (n, m) .= Nothing
                      return $ Just $ ffor' requests $ \(With' l' req) ->
                        let Just rspRaw = Map.lookup l' responses'
                            Aeson.Success rsp = fromJSONViaAllArgsHave req rspRaw
                        in rsp
          result = fmapMaybe fst change
          newState = fmap snd change
          encodeMessages (n, fs) = mconcat $ ffor fs $ \(m, (reqs, _)) -> toListWith' (\(With' l r) -> LBS.toStrict $ encode ((n, m, l), toJSON' r)) reqs
  return (result, state)
