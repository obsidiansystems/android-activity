{-# LANGUAGE JavaScriptFFI, ForeignFunctionInterface, CPP #-}
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
import Safe

storageGet :: (MonadWidget t m, ToJSString k, FromJSString v) => Event t k -> m (Event t (Maybe v))
storageGet k = do
  wv <- askWebView
  performEvent $ fmap (get wv) k
  where
    get wv k = do
      s <- liftIO $ domWindowGetLocalStorage wv
      maybe (return Nothing) (flip getItem k) s


storageRead :: (Read a, MonadWidget t m) => Event t String -> m (Event t (Maybe a))
storageRead k = do
  r <- storageGet k
  return $ fmap (join . fmap readMay) r

storageSet :: (MonadWidget t m, ToJSString k, ToJSString v) => Event t (k, v) -> m ()
storageSet kv = do
  wv <- askWebView
  performEvent_ $ fmap (\(k, v) -> set wv k v) kv
  where
    set wv k v = do
      s <- liftIO $ domWindowGetLocalStorage wv
      case s of
           Nothing -> return ()
           Just s' -> liftIO $ storageSetItem s' (toJSString k) (toJSString v)


storageRemove :: (MonadWidget t m, ToJSString k)  => Event t k -> m ()
storageRemove k = do
  wv <- askWebView
  performEvent_ $ fmap (remove wv) k
  where
    remove wv k = do
      s <- liftIO $ domWindowGetLocalStorage wv
      case s of
           Nothing -> return ()
           Just s' -> liftIO $ storageRemoveItem s' (toJSString k)

storageGetMany :: (MonadWidget t m, ToJSString k, Ord k, FromJSString v) => Event t [k] -> m (Event t (Map k (Maybe v)))
storageGetMany ks = do
  wv <- askWebView
  performEvent $ fmap (getMany wv) ks
  where
    getMany wv ks = do
      s <- liftIO $ domWindowGetLocalStorage wv
      case s of
           Nothing -> return $ Map.fromList $ map (\k -> (k, Nothing)) ks
           Just s' -> liftM Map.fromList $ forM ks $ \k -> do
             i <- getItem s' k
             return (k, i)

storageSetMany :: (MonadWidget t m, ToJSString k, ToJSString v) => Event t (Map k v) -> m ()
storageSetMany is = do
  wv <- askWebView
  performEvent_ $ fmap (setMany wv) is
  where
    setMany wv is = do
      s <- liftIO $ domWindowGetLocalStorage wv
      case s of
           Just s' -> void $ forM (Map.toList is) $ \(k, i) -> liftIO $ storageSetItem s' (toJSString k) (toJSString i)
           _ -> return ()

storageRemoveMany :: (MonadWidget t m, ToJSString k) => Event t [k] -> m ()
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

