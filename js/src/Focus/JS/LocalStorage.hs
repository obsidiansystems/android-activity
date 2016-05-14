{-# LANGUAGE GADTs, FlexibleContexts, TemplateHaskell, RankNTypes #-}
module Focus.JS.LocalStorage where

{-
import Foreign.JavaScript.TH
import GHCJS.DOM.Window (getLocalStorage)
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Storage
import GHCJS.DOM.Types hiding (Event)
import Reflex.Dom hiding (Value)
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Identity

--TODO: This should really take a Storage as an argument, but on webkitgtk, this requires converting the Storage object to its JS wrapper
importJS Unsafe "localStorage['getItem'](this[0])" "localStorageGetItemMaybe" [t| forall x m. MonadJS x m => String -> m (Maybe String) |]

askDomWindow :: (HasWebView m, MonadIO m) => m Window
askDomWindow = do
  wv <- askWebView
  Just doc <- liftIO $ webViewGetDomDocument $ unWebViewSingleton wv
  Just window <- liftIO $ getDefaultView doc
  return window

askLocalStorage :: (HasWebView m, MonadIO m) => m Storage
askLocalStorage = do
  dw <- askDomWindow
  Just s <- liftIO $ getLocalStorage dw
  return s

type Key = String

type Value = String

storageGetInitial :: (HasWebView m, HasJS x m) => Key -> m (Maybe Value)
storageGetInitial = liftM runIdentity . storageGetManyInitial . Identity

storageGetManyInitial :: (HasWebView m, HasJS x m, Traversable f) => f Key -> m (f (Maybe Value))
storageGetManyInitial keys = do
  liftJS $ forM keys localStorageGetItemMaybe

storageGet :: (MonadWidget t m, HasJS x (WidgetHost m)) => Event t (Key) -> m (Event t (Maybe Value))
storageGet = liftM (fmap runIdentity) . storageGetMany . fmap Identity

storageGetMany :: (MonadWidget t m, HasJS x (WidgetHost m), Traversable f) => Event t (f Key) -> m (Event t (f (Maybe Value)))
storageGetMany gets = do
  performEvent $ ffor gets $ liftJS . mapM localStorageGetItemMaybe

storageSet :: (MonadWidget t m) => Event t (Key, Maybe Value) -> m (Event t ())
storageSet = liftM (fmap runIdentity) . storageSetMany . fmap Identity

storageSetMany :: (MonadWidget t m, Traversable f) => Event t (f (Key, Maybe Value)) -> m (Event t (f ()))
storageSetMany sets = do
  dw <- askDomWindow
  Just s <- liftIO $ getLocalStorage dw
  performEvent $ ffor sets $ mapM $ \(k, mv) -> liftIO $ case mv of
    Nothing -> removeItem s k
    Just v -> setItem s k v

storageRemoveAll :: MonadWidget t m => Event t () -> m ()
storageRemoveAll e = do
  dw <- askDomWindow
  s <- liftIO $ getLocalStorage dw
  performEvent_ $ fmap (const $ maybe (return ()) (liftIO . clear) s) e
-}
