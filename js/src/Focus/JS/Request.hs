{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, GADTs, ScopedTypeVariables, FunctionalDependencies, RecursiveDo, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, EmptyDataDecls, NoMonomorphismRestriction, TemplateHaskell, PolyKinds, TypeOperators, DeriveFunctor, LambdaCase, CPP, ForeignFunctionInterface, JavaScriptFFI, DeriveDataTypeable, ConstraintKinds #-}
module Focus.JS.Request where

import Focus.Request

import GHCJS.DOM
import GHCJS.DOM.DOMWindow
import GHCJS.DOM.Types hiding (Event)
import GHCJS.DOM.Document
import GHCJS.DOM.Node
import GHCJS.DOM.NamedNodeMap
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLDocument
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.HTMLSelectElement
import GHCJS.DOM.HTMLOptionElement
import GHCJS.DOM.MouseEvent
import GHCJS.DOM.UIEvent
import GHCJS.DOM.NodeList
import GHCJS.DOM.Event hiding (Event)
import GHCJS.DOM.EventM (event, Signal (..), preventDefault)
import qualified GHCJS.DOM.EventM as GHCJS
import GHCJS.DOM.EventTargetClosures
import Data.Set (Set)
import qualified Data.Set as Set
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import Data.Monoid
import Data.Time.Clock
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.String
import Data.Aeson
import Control.Concurrent.MVar
import Control.Monad

-- Note: The C preprocessor will fail if you use a single-quote in the name
#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

JS(namedNodeMapGetNames_, "(function(){var a = []; for(var i = 0; i < $1.length; i++) { a.push($1[i].name); }; return a;})()", JSRef NamedNodeMap -> IO (JSArray a))

namedNodeMapGetNames :: (IsNamedNodeMap self) => self -> IO (Set String)
namedNodeMapGetNames self = do
  names <- fromArray =<< namedNodeMapGetNames_ (unNamedNodeMap $ toNamedNodeMap self)
  return $ Set.fromList $ map fromJSString names

validJSRef :: JSRef a -> Maybe (JSRef a)
validJSRef r = if isUndefined r || isNull r then Nothing else Just r

JS(getScrollTop_, "$1.scrollTop", JSRef HTMLElement -> IO (JSRef Int))
getScrollTop :: IsHTMLElement e => e -> IO Int
getScrollTop e = do
  x <- getScrollTop_ $ unHTMLElement $ toHTMLElement e
  Just d <- fromJSRef x
  return d

JS(getScrollLeft_, "$1.scrollLeft", JSRef HTMLElement -> IO (JSRef Int))

getScrollLeft :: IsHTMLElement e => e -> IO Int
getScrollLeft e = do
  x <- getScrollLeft_ $ unHTMLElement $ toHTMLElement e
  Just d <- fromJSRef x
  return d

--TODO: These should not be using decodeURIComponent; these fields should be parsed first, *then* decodeURICompoent should be called on individual values
--TODO: These should have IO in the raw versions as well
JS(getWindowLocationHash_, "decodeURIComponent(window.location.hash)", JSRef String)
getWindowLocationHash :: IO String
getWindowLocationHash = do
  Just h <- fromJSRef $ getWindowLocationHash_
  return h

JS(getWindowLocationSearch_, "decodeURIComponent(window.location.search)", JSRef String)
getWindowLocationSearch :: IO String
getWindowLocationSearch = do
  Just h <- fromJSRef $ getWindowLocationSearch_
  return h

JS(windowClose, "window.close()", IO ())

JS(notificationRequestPermission_, "Notification.requestPermission()", IO ())
JS(notificationCheckPermission_, "Notification.checkPermission()", IO (JSRef Int))
JS(newNotification_, "new Notification($1, {body: $2, icon: $3})", JSRef String -> JSRef String -> JSRef String -> IO ())
newDesktopNotification :: String -> String -> String -> IO ()
newDesktopNotification title body icon = do
  t <- toJSRef title
  b <- toJSRef body
  i <- toJSRef icon
  newNotification_ t b i

timeFrom t ct =
  let d = round $ diffUTCTime ct t
  in describe d
  where
    describeAbs :: Integer -> String
    describeAbs n
      | n >= 86400 = let days = n `Prelude.div` 86400 in show days <> " days "
      | n >= 3600 = let hrs = n `Prelude.div` 3600 in show hrs <> " hours "
      | n >= 60 = let mins = n `Prelude.div` 60 in show mins <> " minutes "
      | n > 0 = show n <> " seconds "
      | otherwise = ""
    describe :: Integer -> String
    describe n = if n > 0
                    then describeAbs n <> "ago"
                    else describeAbs (abs n) <> "from now"

JS(consoleLog, "console.log($1)", JSRef a -> IO ())
JS(documentContains_, "$1.contains($2)", JSRef Document -> JSRef Element -> IO JSBool)
documentContains :: (IsDocument self, IsElement e) => self -> e -> IO Bool
documentContains doc e = fmap fromJSBool $ documentContains_ (unDocument (toDocument doc)) (unElement (toElement e))
JS(eval, "eval($1)", JSString -> IO ())

JS(setStyle_, "$1.style[$2] = $3", JSRef HTMLElement -> JSString -> JSString -> IO ())
setStyle el attr val = setStyle_ (unHTMLElement (toHTMLElement el)) (toJSString attr) (toJSString val)

syncApi :: (Request r, ToJSON a, FromJSON a) => r a -> IO a
syncApi req = do
  v <- newEmptyMVar
  asyncApi req $ putMVar v --TODO: Error handling
  takeMVar v

asyncApi :: (Request r, ToJSON a, FromJSON a) => r a -> (a -> IO b) -> IO ()
asyncApi r f = do
  let reqJson = encode $ SomeRequest r
  _ <- mkPost "/api" (fromString $ T.unpack $ decodeUtf8 $ LBS.toStrict reqJson) $ \rspJson -> do
    Just rsp <- return $ decodeValue' $ LBS.fromStrict $ encodeUtf8 $ fromJSString $ rspJson
    f rsp
  return ()

data XMLHttpRequest
JS(newXhr, "new XMLHttpRequest()", IO (JSRef XMLHttpRequest))
JS(xhrOpen, "($1).open(($2), ($3), ($4))", JSRef XMLHttpRequest -> JSString -> JSString -> JSBool -> IO ())
JS(xhrSend, "($1).send()", JSRef XMLHttpRequest -> IO ())
JS(xhrSendWithData, "($1).send(($2))", JSRef XMLHttpRequest -> JSString -> IO ())
JS(xhrSetOnReadyStateChange, "($1).onreadystatechange = ($2)", JSRef XMLHttpRequest -> JSFun (IO ()) -> IO ())
JS(xhrGetReadyState, "($1).readyState", JSRef XMLHttpRequest -> IO (JSRef Int))
JS(xhrGetResponseText, "($1).responseText", JSRef XMLHttpRequest -> IO (JSString))
JS(xhrGetResponse, "($1).response", JSRef XMLHttpRequest -> IO (JSRef a))
JS(xhrSetResponseType, "($1).responseType = $2", JSRef XMLHttpRequest -> JSString -> IO ())

mkRequest :: String -> (JSRef XMLHttpRequest -> IO ()) -> String -> (JSString -> IO a) -> IO (JSRef XMLHttpRequest)
mkRequest = mkRequestGeneric Nothing (xhrGetResponseText)

mkGet :: String -> (JSString -> IO a) -> IO (JSRef XMLHttpRequest)
mkGet = mkRequest "GET" xhrSend

mkPost :: String -> JSString -> (JSString -> IO a) -> IO (JSRef XMLHttpRequest)
mkPost url d = mkRequest "POST" (flip xhrSendWithData d) url

mkBinaryRequest :: String -> (JSRef XMLHttpRequest -> IO ()) -> String -> (ByteString -> IO a) -> IO (JSRef XMLHttpRequest)
mkBinaryRequest = mkRequestGeneric (Just "arraybuffer") (bufferByteString 0 0 <=< xhrGetResponse)

mkBinaryGet :: String -> (ByteString -> IO a) -> IO (JSRef XMLHttpRequest)
mkBinaryGet = mkBinaryRequest "GET" xhrSend

mkRequestGeneric :: Maybe String -> (JSRef XMLHttpRequest -> IO r) -> String -> (JSRef XMLHttpRequest -> IO ()) -> String -> (r -> IO a) -> IO (JSRef XMLHttpRequest)
mkRequestGeneric responseType convertResponse method send url cb = do
  xhr <- newXhr
  xhrOpen xhr (fromString method) (fromString url) jsTrue
  maybe (return ()) (xhrSetResponseType xhr . toJSString) responseType
  rec callback <- syncCallback AlwaysRetain True $ do
        readyState <- fromJSRef =<< xhrGetReadyState xhr
        if readyState == Just 4
           then do
             _ <- cb =<< convertResponse xhr
             release callback --TODO: We're assuming that the request will only be called with readyState == 4 once; is this valid?
           else return ()
  xhrSetOnReadyStateChange xhr callback
  _ <- send xhr
  return xhr
