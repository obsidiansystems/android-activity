{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, TemplateHaskell, NoMonomorphismRestriction, EmptyDataDecls, RankNTypes, GADTs, RecursiveDo, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveDataTypeable, GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, UndecidableInstances, PolyKinds, PartialTypeSignatures, AllowAmbiguousTypes, OverloadedStrings, PatternGuards #-}

module Focus.JS.WebSocket where

import Focus.Request

import Prelude hiding (div, span, mapM, mapM_, concat, concatMap, all, sequence)

import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Semigroup hiding (option)
import qualified Data.ByteString.Lazy as LBS

import Reflex
import Reflex.Dom hiding (webSocket)
import qualified Reflex.Dom.WebSocket as RDWS

import Control.Lens

import Data.Aeson (encode, decodeStrict', FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Constraint
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import Debug.Trace.LocationTH
#ifdef __GHCJS__
import GHCJS.Types (JSVal)
import GHCJS.Marshal
import Control.Exception (try, SomeException)
import System.IO.Unsafe
import JavaScript.JSON.Types.FromJSVal ()
#endif

newtype JSWebSocket x = JSWebSocket { unWebSocket :: JSRef x }

instance ToJS x (JSWebSocket x) where
  withJS (JSWebSocket r) f = f r

instance FromJS x (JSWebSocket x) where
  fromJS = return . JSWebSocket

data WebSocketUrl = WebSocketUrl
       { _websocket_protocol :: Text
       , _websocket_host :: Text
       , _websocket_port :: Int
       , _websocket_path :: Text
       } deriving (Eq, Ord, Show, Read)

-- | Warning: Only one of these websockets may be opened on a given page in most browsers
webSocket :: forall x t m. (HasJS x m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadIO m, MonadIO (Performable m), HasWebView m) => Either WebSocketUrl Text -> WebSocketConfig t Text -> m (WebSocket t)
webSocket murl config
  | Left url <- murl = do
    RDWS.webSocket (mconcat [ _websocket_protocol url, "://"
                            , _websocket_host url, ":", T.pack (show (_websocket_port url))
                            , "/", _websocket_path url ]) config
  | Right path <- murl = do
    pageHost <- liftIO . getLocationHost =<< askWebView
    pageProtocol <- liftIO . getLocationProtocol =<< askWebView
    let wsProtocol = case pageProtocol of
          "http:" -> "ws:"
          "https:" -> "wss:"
          "file:" -> "ws:"
          s -> $failure ("unrecognized wsProtocol: " <> T.unpack s)
        wsHost = case pageProtocol of
          "file:" -> "localhost:8000"
          _ -> pageHost
    RDWS.webSocket (wsProtocol <> "//" <> wsHost <> path) config

monoConst :: a -> a -> a
monoConst a _ = a

fromJSONViaAllArgsHave :: forall rsp req a. AllArgsHave (ComposeConstraint FromJSON rsp) req => req a -> Aeson.Value -> Aeson.Result (rsp a)
fromJSONViaAllArgsHave req rspRaw = case getArgDict req :: Dict (ComposeConstraint FromJSON rsp a) of
  Dict -> fromJSON rspRaw

apiSocket :: forall (x :: *) (m :: * -> *) (t :: *) (f :: (k -> *) -> *) (req :: k -> *) (rsp :: k -> *).
             ( HasJS x m
             , HasJS x (WidgetHost m)
             , HasWebView m
             , MonadFix m
             , MonadHold t m
             , MonadIO m
             , MonadIO (Performable m)
             , PostBuild t m
             , PerformEvent t m
             , TriggerEvent t m
             , Traversable' f
             , ToJSON' req
             , AllArgsHave (ComposeConstraint FromJSON rsp) req
             )
          => Either WebSocketUrl Text
          -> Event t [f req]
          -> m (Event t (f rsp), Dynamic t (Map (Int, Int) (f (With' Int req), Int, Map Int Aeson.Value)))
apiSocket murl batches = do
  batchesWithSerialNumbers <- zipListWithEvent (\n r -> (n, zip [(1 :: Int) ..] $ fmap numberAndSize' r)) [(1 :: Int) ..] batches
  rec ws <- webSocket murl $ def & webSocketConfig_send .~ fmap encodeMessages batchesWithSerialNumbers
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
          encodeMessages (n, fs) = mconcat $ ffor fs $ \(m, (reqs, _)) -> toListWith' (\(With' l r) -> decodeUtf8 $  LBS.toStrict $ encode ((n, m, l), toJSON' r)) reqs
  return (result, state)


#ifdef __GHCJS__
foreign import javascript unsafe "JSON['parse']($1)" js_jsonParse :: JSVal -> JSVal

rawDecode :: (FromJSON a) => JSVal -> Maybe a
rawDecode jsv = do
  -- traceM "customDecode"
  -- TODO pFromJSVal to avoid unsafePerformIO
  let res = unsafePerformIO $ try $ fromJSVal $ js_jsonParse jsv
  case res of
   Left (_e::SomeException) -> do
     -- traceM $ "====================================================================="
     -- traceM $ show e
     -- traceM $ "====================================================================="
     Nothing
   Right (v :: (Maybe Aeson.Value)) -> do
     -- traceM $ show $ js_jsonTypeOf jsv'
     -- traceM $ "Success" ++ show v
     maybe Nothing go v
  where
    go v = case Aeson.fromJSON v of
      Aeson.Success a -> Just a
      _ -> Nothing

rawWebSocket :: forall x t m. (HasJS x m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadIO m, MonadIO (Performable m), HasWebView m) => Either WebSocketUrl Text -> WebSocketConfig t Text -> m (RawWebSocket t JSVal)
rawWebSocket murl config
  | Left url <- murl
  = do
    RDWS.webSocket' (mconcat [ _websocket_protocol url, "://"
                             , _websocket_host url, ":", T.pack (show (_websocket_port url))
                             , _websocket_path url ]) config (either (error "websocket': expected JSVal") id)
  | Right path <- murl = do
    pageHost <- liftIO . getLocationHost =<< askWebView
    pageProtocol <- liftIO . getLocationProtocol =<< askWebView
    let wsProtocol = case pageProtocol of
          "http:" -> "ws:"
          "https:" -> "wss:"
          "file:" -> "ws:"
          s -> $failure ("unrecognized wsProtocol: " <> T.unpack s)
        wsHost = case pageProtocol of
          "file:" -> "localhost:8000"
          _ -> pageHost
    RDWS.webSocket' (wsProtocol <> "//" <> wsHost <> path) config (either (error "websocket': expected JSVal") id)
#endif
