{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, TemplateHaskell, NoMonomorphismRestriction, EmptyDataDecls, RankNTypes, GADTs, RecursiveDo, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveDataTypeable, GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, UndecidableInstances, PolyKinds, PartialTypeSignatures, AllowAmbiguousTypes #-}

module Focus.JS.WebSocket where

import Focus.Request

import Prelude hiding (div, span, mapM, mapM_, concat, concatMap, all, sequence)

import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.IO.Class
import Data.Semigroup hiding (option)
import qualified Data.ByteString.Lazy as LBS

import Reflex
import Reflex.Dom hiding (webSocket, webSocketConfig_send)
import qualified Reflex.Dom.WebSocket as RDWS

import Control.Lens

import Data.Aeson (encode, decodeStrict', FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Constraint

import Foreign.JavaScript.TH

import Debug.Trace.LocationTH

newtype JSWebSocket x = JSWebSocket { unWebSocket :: JSRef x }

instance ToJS x (JSWebSocket x) where
  withJS (JSWebSocket r) f = f r

instance FromJS x (JSWebSocket x) where
  fromJS = return . JSWebSocket

-- Some of this is already in Reflex.Dom.WebSocket.Foreign, but that module isn't exposed.

importJS Unsafe "(function(that) { var ws = new WebSocket(that[0]); ws['binaryType'] = 'arraybuffer'; ws['onmessage'] = function(e){ that[1](e.data); }; ws['onclose'] = function(e){ that[2](); }; return ws; })(this)" "newWebSocket_" [t| forall x m. MonadJS x m => String -> JSFun x -> JSFun x -> m (JSWebSocket x) |]

importJS Unsafe "this[0]['send'](String.fromCharCode.apply(null, this[1]))" "webSocketSend_" [t| forall x m. MonadJS x m => JSWebSocket x -> JSUint8Array x -> m () |]

-- | Warning: Only one of these websockets may be opened on a given page in most browsers
webSocket :: forall x t m. (HasJS x m, HasJS x (WidgetHost m), MonadWidget t m) => String -> WebSocketConfig t -> m (WebSocket t)
webSocket path config = do
  pageHost <- liftIO . getLocationHost =<< askWebView
  pageProtocol <- liftIO . getLocationProtocol =<< askWebView
  let wsProtocol = case pageProtocol of
        "http:" -> "ws:"
        "https:" -> "wss:"
        "file:" -> "ws:"
        s -> $failure ("unrecognized wsProtocol: " <> s)
      wsHost = case pageProtocol of
        "file:" -> "localhost:8000"
        _ -> pageHost
  RDWS.webSocket (wsProtocol <> "//" <> wsHost <> path) config

makeLensesWith (lensRules & simpleLenses .~ True) ''WebSocketConfig

monoConst :: a -> a -> a
monoConst a _ = a

fromJSONViaAllArgsHave :: forall rsp req a. AllArgsHave (ComposeConstraint FromJSON rsp) req => req a -> Aeson.Value -> Aeson.Result (rsp a)
fromJSONViaAllArgsHave req rspRaw = case getArgDict req :: Dict (ComposeConstraint FromJSON rsp a) of
  Dict -> fromJSON rspRaw

apiSocket :: forall (x :: *) (m :: * -> *) (t :: *) (f :: (k -> *) -> *) (req :: k -> *) (rsp :: k -> *).
             ( HasJS x m
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
  rec ws <- webSocket path $ def & webSocketConfig_send .~ fmap encodeMessages batchesWithSerialNumbers
      state <- foldDyn ($) mempty $ mergeWith (.) [ fmap (\(n, fs) -> foldl (.) id $ map (\(m, (r, sz)) -> Map.insertWith (error "apiSocket: adding an existing serial number to the outgoing request buffer") (n, m) (r, sz, mempty :: Map Int Aeson.Value)) fs) batchesWithSerialNumbers
                                                  , fmap snd change
                                                  ]
      let change = flip push (_webSocket_recv ws) $ \msg -> liftM Just $ do
            oldState <- sample $ current state
            let Just ((n, m, l), rsp) = decodeStrict' msg
                Just (requests, sz, responses) = Map.lookup (n, m) oldState
                responses' = Map.insertWith (error "apiSocket: received a response with the same tags as another pending response") l rsp responses
            case Map.size responses' `compare` sz of
              LT -> do
                return (Nothing, at (n, m) .~ Just (requests, sz, responses'))
              EQ -> do
                let finishedResponses = Just $ ffor' requests $ \(With' l' (req :: req z)) ->
                      let Just rspRaw = Map.lookup l' responses'
                          Aeson.Success rsp' = fromJSONViaAllArgsHave req rspRaw
                      in rsp'
                return (finishedResponses, at (n, m) .~ Nothing)
              GT -> $undef
          result = fmapMaybe fst change
          encodeMessages (n, fs) = mconcat $ ffor fs $ \(m, (reqs, _)) -> toListWith' (\(With' l r) -> LBS.toStrict $ encode ((n, m, l), toJSON' r)) reqs
  return (result, state)
