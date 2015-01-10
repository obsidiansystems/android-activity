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
import Control.Monad.Ref

import Reflex
import Reflex.Dom
import Reflex.Host.Class --TODO

import Debug.Trace (trace)

import System.IO.Unsafe

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

data ArrayBuffer
 
newtype WebSocket = WebSocket { unWebSocket :: JSRef WebSocket }
JS(newWebSocket_, "$r = new WebSocket($1); $r.binaryType = 'arraybuffer'; $r.onmessage = function(e){ $2(e.data); }; $r.onclose = function(e){ $3(); };", JSString -> JSFun (JSRef ArrayBuffer -> IO ()) -> JSFun (IO ()) -> IO (JSRef WebSocket))

newWebSocket :: String -> (ByteString -> IO ()) -> IO () -> IO WebSocket
newWebSocket url onMessage onClose = do
  onMessageFun <- syncCallback1 AlwaysRetain True $ onMessage <=< bufferByteString 0 0
  rec onCloseFun <- syncCallback AlwaysRetain True $ do
        release onMessageFun
        release onCloseFun
        onClose
  liftM WebSocket $ newWebSocket_ (toJSString url) onMessageFun onCloseFun

JS(getLocationHost_, "location.host", IO JSString)

getLocationHost = liftM fromJSString getLocationHost_

JS(getLocationProtocol_, "location.protocol", IO JSString)

getLocationProtocol = liftM fromJSString getLocationProtocol_

-- | Warning: Only one of these websockets may be opened on a given page in most browsers
webSocket :: MonadWidget t m => String -> m (Event t ByteString)
webSocket path = do
  host <- liftIO getLocationHost
  pageProtocol <- liftIO getLocationProtocol
  let wsProtocol = case pageProtocol of
        "http:" -> "ws:"
        "https:" -> "wss:"
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  (eRecv, eRecvTriggerRef) <- newEventWithTriggerRef
  --TODO: reconnect if connection dropped
  --TODO: Disconnect if value no longer needed
  let onMessage m = do
        print m
        maybe (return ()) (\t -> postGui $ runWithActions [t :=> m]) =<< readRef eRecvTriggerRef
      start = do
        void $ newWebSocket (wsProtocol <> "//" <> host <> path) onMessage (void $ forkIO $ threadDelay 1000000 >> start) --TODO: Is the forkIO necessary, or do event handlers run in their own threads automatically?
  liftIO start
  return eRecv
