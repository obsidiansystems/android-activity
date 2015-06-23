{-# LANGUAGE GADTs, FlexibleContexts, TemplateHaskell, RankNTypes #-}
module Focus.JS.LocalStorage
       ( storageGet
       , storageSet
       , storageRemove
       , storageGetMany
       , storageSetMany
       , storageRemoveMany
       , storageRemoveAll
       , storageRead
       ) where

import Foreign.JavaScript.TH
import GHCJS.DOM.DOMWindow (domWindowGetLocalStorage)
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Storage
import GHCJS.DOM.Types hiding (Event)
import Reflex.Dom
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Map as Map
import Data.Map (Map)
import Safe

--TODO: This should really take a Storage as an argument
importJS Unsafe "localStorage['getItem'](this[1])" "localStorageGetItemMaybe" [t| forall x m. MonadJS x m => String -> m (Maybe String) |]

storageGet :: (HasJS x (WidgetHost m), MonadWidget t m) => Event t String -> m (Event t (Maybe String))
storageGet key = do
  performEvent $ fmap (liftJS . localStorageGetItemMaybe) key

askDomWindow :: MonadWidget t m => m DOMWindow
askDomWindow = do
  wv <- askWebView
  Just doc <- liftIO $ webViewGetDomDocument $ unWebViewSingleton wv
  Just window <- liftIO $ documentGetDefaultView doc
  return window

--storageRead :: (Read a, MonadWidget t m) => Event t String -> m (Event t (Maybe a))
storageRead k = do
  r <- storageGet k
  return $ fmap (join . fmap readMay) r

--storageSet :: (MonadWidget t m, ToJSString k, ToJSString v) => Event t (k, v) -> m ()
storageSet kv = do
  dw <- askDomWindow
  performEvent_ $ fmap (\(k, v) -> set dw k v) kv
  where
    set dw k v = do
      s <- liftIO $ domWindowGetLocalStorage dw
      case s of
           Nothing -> return ()
           Just s' -> liftIO $ storageSetItem s' k v

--storageRemove :: (MonadWidget t m, ToJSString k)  => Event t k -> m ()
storageRemove k = do
  dw <- askDomWindow
  performEvent_ $ fmap (remove dw) k
  where
    remove dw k = do
      s <- liftIO $ domWindowGetLocalStorage dw
      case s of
           Nothing -> return ()
           Just s' -> liftIO $ storageRemoveItem s' k

--storageGetMany :: (MonadWidget t m, ToJSString k, Ord k, FromJSString v) => Event t [k] -> m (Event t (Map k (Maybe v)))
storageGetMany ks = do
  dw <- askDomWindow
  performEvent $ fmap (getMany dw) ks
  where
    getMany dw ks = do
      s <- liftIO $ domWindowGetLocalStorage dw
      case s of
           Nothing -> return $ Map.fromList $ map (\k -> (k, Nothing)) ks
           Just s' -> liftM Map.fromList $ forM ks $ \k -> do
             i <- storageGetItem s' k
             return (k, i)

--storageSetMany :: (MonadWidget t m, ToJSString k, ToJSString v) => Event t (Map k v) -> m ()
storageSetMany is = do
  dw <- askDomWindow
  performEvent_ $ fmap (setMany dw) is
  where
    setMany dw is = do
      s <- liftIO $ domWindowGetLocalStorage dw
      case s of
           Just s' -> void $ forM (Map.toList is) $ \(k, i) -> liftIO $ storageSetItem s' k i
           _ -> return ()

--storageRemoveMany :: (MonadWidget t m, ToJSString k) => Event t [k] -> m ()
storageRemoveMany ks = do
  dw <- askDomWindow
  performEvent_ $ ffor ks $ \ks' -> do
    s <- liftIO $ domWindowGetLocalStorage dw
    case s of
         Just s' -> void $ forM ks' $ \k -> liftIO $ storageRemoveItem s' k
         _ -> return ()

--storageRemoveAll :: MonadWidget t m => Event t () -> m ()
storageRemoveAll e = do
  dw <- askDomWindow
  s <- liftIO $ domWindowGetLocalStorage dw
  performEvent_ $ fmap (const $ maybe (return ()) (liftIO . storageClear) s) e
