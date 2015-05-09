{-# LANGUAGE JavaScriptFFI, ForeignFunctionInterface, CPP #-}
module Focus.JS.LocalStorage
       ( storageGet
       , storageSet
       , storageRemove
       , storageGetMany
       , storageSetMany
       , storageRemoveMany
       , storageRemoveAll
       ) where

import GHCJS.DOM.DOMWindow (domWindowGetLocalStorage)
import GHCJS.DOM.Storage
import GHCJS.DOM.Types hiding (Event)
import GHCJS.Types
import Reflex.Dom
import Control.Monad.IO.Class
import GHCJS.Foreign
import Control.Monad
import qualified Data.Map as Map
import Data.Map (Map)

storageGet :: MonadWidget t m => Event t String -> m (Event t (Maybe String))
storageGet k = do
  wv <- askWebView
  performEvent $ fmap (get wv) k
  where
    get wv k = do
      s <- liftIO $ domWindowGetLocalStorage wv
      maybe (return Nothing) (flip getItem k) s


storageSet :: MonadWidget t m => Event t (String, String) -> m ()
storageSet kv = do
  wv <- askWebView
  performEvent_ $ fmap (\(k,v) -> set wv k v) kv
  where
    set wv k v = do
      s <- liftIO $ domWindowGetLocalStorage wv
      case s of
           Nothing -> return ()
           Just s' -> liftIO $ storageSetItem s' (toJSString k) (toJSString v)


storageRemove :: MonadWidget t m => Event t String -> m ()
storageRemove k = do
  wv <- askWebView
  performEvent_ $ fmap (remove wv) k
  where
    remove wv k = do
      s <- liftIO $ domWindowGetLocalStorage wv
      case s of
           Nothing -> return ()
           Just s' -> liftIO $ storageRemoveItem s' (toJSString k)

storageGetMany :: MonadWidget t m => Event t [String] -> m (Event t (Map String (Maybe String)))
storageGetMany ks = do
  wv <- askWebView
  performEvent $ fmap (getMany wv) ks
  where
    getMany wv ks = do
      s <- liftIO $ domWindowGetLocalStorage wv
      case s of
           Nothing -> return $ Map.fromList $ map (\k -> (k,Nothing)) ks
           Just s' -> liftM Map.fromList $ forM ks $ \k -> do
             i <- getItem s' k
             return (k, i)

storageSetMany :: MonadWidget t m => Event t (Map String String) -> m ()
storageSetMany is = do
  wv <- askWebView
  performEvent_ $ fmap (setMany wv) is
  where
    setMany wv is = do
      s <- liftIO $ domWindowGetLocalStorage wv
      case s of
           Just s' -> void $ forM (Map.toList is) $ \(k,i) -> liftIO $ storageSetItem s' (toJSString k) (toJSString i)
           _ -> return ()

storageRemoveMany :: MonadWidget t m => Event t [String] -> m ()
storageRemoveMany ks = do
  wv <- askWebView
  performEvent_ $ ffor ks $ \ks' -> do
    s <- liftIO $ domWindowGetLocalStorage wv
    case s of
         Just s' -> void $ forM ks' $ \k -> liftIO $ storageRemoveItem s' (toJSString k)
         _ -> return ()


storageRemoveAll :: MonadWidget t m => Event t () -> m ()
storageRemoveAll e = do
  wv <- askWebView
  s <- liftIO $ domWindowGetLocalStorage wv
  performEvent_ $ fmap (const $ maybe (return ()) (liftIO . storageClear) s) e

getItem s k = do
  i <- liftIO $ ghcjs_dom_storage_get_item (unStorage (toStorage s)) (toJSString k)
  if isNull i then return Nothing else return $ Just (fromJSString i)

