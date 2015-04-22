{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, TemplateHaskell, NoMonomorphismRestriction, EmptyDataDecls, RankNTypes, GADTs, RecursiveDo, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveDataTypeable, GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, UndecidableInstances #-}

module Focus.JS.WebSocket where
import Prelude hiding (div, span, mapM, mapM_, concatMap, concat, all, sequence)
import qualified Prelude

import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.IO.Class
import GHCJS.Foreign
import GHCJS.Types
import Control.Concurrent
import Data.Semigroup hiding (option)
import Data.Dependent.Map (DMap, GCompare (..), GOrdering (..), DSum (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad.Ref
import Data.Text.Encoding

import Reflex
import Reflex.Dom
import Reflex.Host.Class --TODO

import Data.Default

import Data.IORef
import Control.Lens

import Foreign.Ptr

import Debug.Trace (trace)

import System.IO.Unsafe

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

newtype WebSocket = WebSocket { unWebSocket :: JSRef WebSocket }
JS(newWebSocket_, "$r = new WebSocket($1); $r.binaryType = 'arraybuffer'; $r.onmessage = function(e){ $2(e.data); }; $r.onclose = function(e){ $3(); };", JSString -> JSFun (JSString -> IO ()) -> JSFun (IO ()) -> IO (JSRef WebSocket))
JS(webSocketSend_, "$1['send']($2)", JSRef WebSocket -> JSRef JSByteArray -> IO ()) --TODO: Use (JSRef ArrayBuffer) instead of JSString

data JSByteArray
JS(extractByteArray, "new Uint8Array($1_1.buf, $1_2, $2)", Ptr a -> Int -> IO (JSRef JSByteArray))

newWebSocket :: String -> (ByteString -> IO ()) -> IO () -> IO WebSocket
newWebSocket url onMessage onClose = do
  onMessageFun <- syncCallback1 AlwaysRetain True $ onMessage <=< bufferByteString 0 0
  rec onCloseFun <- syncCallback AlwaysRetain True $ do
        release onMessageFun
        release onCloseFun
        onClose
  liftM WebSocket $ newWebSocket_ (toJSString url) onMessageFun onCloseFun

webSocketSend :: WebSocket -> ByteString -> IO ()
webSocketSend ws payload = BS.useAsCString payload $ \cStr -> do
  ba <- extractByteArray cStr $ BS.length payload
  webSocketSend_ (unWebSocket ws) ba

JS(getLocationHost_, "location.host", IO JSString)

getLocationHost = liftM fromJSString getLocationHost_

JS(getLocationProtocol_, "location.protocol", IO JSString)

getLocationProtocol = liftM fromJSString getLocationProtocol_

data WebSocketConfig t
   = WebSocketConfig { _webSocketConfig_send :: Event t ByteString
                     }



instance Reflex t => Default (WebSocketConfig t) where
  def = WebSocketConfig never

-- | Warning: Only one of these websockets may be opened on a given page in most browsers
webSocket :: MonadWidget t m => String -> WebSocketConfig t -> m (Event t ByteString)
webSocket path config = do
  host <- liftIO getLocationHost
  pageProtocol <- liftIO getLocationProtocol
  let wsProtocol = case pageProtocol of
        "http:" -> "ws:"
        "https:" -> "wss:"
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  (eRecv, eRecvTriggerRef) <- newEventWithTriggerRef
  currentSocketRef <- liftIO $ newIORef Nothing
  --TODO: Disconnect if value no longer needed
  let onMessage m = do
        maybe (return ()) (\t -> postGui $ runWithActions [t :=> m]) =<< readRef eRecvTriggerRef
      start = do
        ws <- newWebSocket (wsProtocol <> "//" <> host <> path) onMessage $ do
          void $ forkIO $ do --TODO: Is the forkIO necessary, or do event handlers run in their own threads automatically?
            writeIORef currentSocketRef Nothing
            threadDelay 1000000
            start
        writeIORef currentSocketRef $ Just ws
        return ()
  liftIO start
  performEvent_ $ ffor (_webSocketConfig_send config) $ \payload -> liftIO $ do
    mws <- readIORef currentSocketRef
    case mws of
      Nothing -> return () -- Discard --TODO: should we do something better here? probably buffer it, since we handle reconnection logic; how do we verify that the server has received things?
      Just ws -> do
        webSocketSend ws payload
  return eRecv

makeLensesWith (lensRules & simpleLenses .~ True) ''WebSocketConfig
