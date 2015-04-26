{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, TemplateHaskell, NoMonomorphismRestriction, EmptyDataDecls, RankNTypes, GADTs, RecursiveDo, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveDataTypeable, GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, UndecidableInstances, PolyKinds #-}

module Focus.JS.WebSocket where

import Focus.Request

import Prelude hiding (div, span, mapM, mapM_, concat, concatMap, all, sequence)
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
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.Ref
import Data.Text.Encoding

import Reflex
import Reflex.Dom
import Reflex.Host.Class --TODO

import Data.Default

import Data.IORef
import Control.Lens

import Data.Aeson (encode, decodeStrict', toJSON, parseJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parse)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Applicative
import Data.Align
import Data.These

import Foreign.Ptr

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

newtype JSWebSocket = JSWebSocket { unWebSocket :: JSRef JSWebSocket }
JS(newWebSocket_, "$r = new JSWebSocket($1); $r.binaryType = 'arraybuffer'; $r.onmessage = function(e){ $2(e.data); }; $r.onclose = function(e){ $3(); };", JSString -> JSFun (JSString -> IO ()) -> JSFun (IO ()) -> IO (JSRef JSWebSocket))
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

data WebSocketConfig t
   = WebSocketConfig { _webSocketConfig_send :: Event t [ByteString]
                     }

instance Reflex t => Default (WebSocketConfig t) where
  def = WebSocketConfig never

data WebSocket t
   = WebSocket { _webSocket_recv :: Event t ByteString
               }

-- | Warning: Only one of these websockets may be opened on a given page in most browsers
webSocket :: MonadWidget t m => String -> WebSocketConfig t -> m (WebSocket t)
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
  performEvent_ $ ffor (_webSocketConfig_send config) $ \payloads -> liftIO $ forM_ payloads $ \payload -> do
    mws <- readIORef currentSocketRef
    case mws of
      Nothing -> return () -- Discard --TODO: should we do something better here? probably buffer it, since we handle reconnection logic; how do we verify that the server has received things?
      Just ws -> do
        webSocketSend ws payload
  return $ WebSocket eRecv

makeLensesWith (lensRules & simpleLenses .~ True) ''WebSocketConfig

class Functor' (f :: (k -> *) -> *) where
  fmap' :: (forall x. a x -> b x) -> f a -> f b

ffor' :: Functor' f => f a -> (forall x. a x -> b x) -> f b
ffor' x f = fmap' f x

class Foldable' f where
  foldr' :: (forall x. a x -> b -> b) -> b -> f a -> b

mapM_' :: (Foldable' f, Monad m) => (forall x. a x -> m b) -> f a -> m ()
mapM_' f = foldr' ((>>) . f) (return ())

forM_' :: (Foldable' f, Monad m) => f a -> (forall x. a x -> m b) -> m ()
forM_' xs f = mapM_' f xs

toListWith' :: Foldable' f => (forall x. a x -> b) -> f a -> [b]
toListWith' f t = foldr' ((:) . f) [] t

class (Functor' t, Foldable' t) => Traversable' (t :: (k -> *) -> *) where
  mapM' :: Monad m => (forall x. a x -> m (b x)) -> t a -> m (t b)

data With' a (f :: k -> *) (x :: k) = With' a (f x)

-- | Sequentially number the contents of the Traversable'
numberAndSize' :: forall t a. Traversable' t => t a -> (t (With' Int a), Int)
numberAndSize' t = runState (mapM' f t) 1
  where f :: forall x. a x -> State Int (With' Int a x)
        f a = do
          n <- get
          modify succ
          return $ With' n a

class ToJSON' f where
  toJSON' :: f a -> Aeson.Value

class FromJSON' f where
  parseJSON' :: Aeson.Value -> Parser (f a)

fromJSON' :: FromJSON' f => Aeson.Value -> Aeson.Result (f a)
fromJSON' = parse parseJSON'

apiSocket :: forall t m (f :: (k -> *) -> *) (req :: k -> *) (rsp :: k -> *).
             ( MonadWidget t m
             , Traversable' f
             , ToJSON' req
             , FromJSON' rsp
             )
          => String
          -> Event t [f req]
          -> m (Event t [f rsp])
apiSocket path batches = do
  batchesWithSerialNumbers <- zipListWithEvent (\n r -> (n, zip [(1 :: Int) ..] $ fmap numberAndSize' r)) [(1 :: Int) ..] batches
  rec let change = flip push (align batchesWithSerialNumbers $ _webSocket_recv ws) $ \update -> liftM Just $ do
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
                  case Map.size responses `compare` sz of
                    LT -> do
                      at (n, m) .= Just (requests, sz, responses')
                      return Nothing
                    EQ -> do
                      at (n, m) .= Nothing
                      return $ Just $ (:[]) $ ffor' requests $ \(With' l' req) ->
                        let Just rspRaw = Map.lookup l' responses'
                            Aeson.Success rsp = fromJSON' rspRaw
                        in rsp
          result = fmapMaybe fst change
          newState = fmap snd change
      state <- holdDyn mempty newState
      let encodeMessages :: (Int, [(Int, (f (With' Int req), Int))]) -> [ByteString]
          encodeMessages (n, fs) = mconcat $ ffor fs $ \(m, (reqs, _)) -> toListWith' (\(With' l r) -> LBS.toStrict $ encode ((n, m, l), toJSON' r)) reqs
      ws <- webSocket path $ def & webSocketConfig_send .~ fmap encodeMessages batchesWithSerialNumbers
  return result
