{-# LANGUAGE GADTs, FlexibleContexts, TemplateHaskell, RankNTypes #-}
module Focus.JS.LocalStorage where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Identity
import Data.Aeson hiding (Value)
import Data.ByteString.Lazy as LBS
import Data.Text as T
import Data.Text.Encoding as T
import Focus.JS.Window
import Focus.Request
import Foreign.JavaScript.TH
import GHCJS.DOM.Storage
import GHCJS.DOM.Window (getLocalStorage)
import Reflex.Dom hiding (Value)

--TODO: This should really take a Storage as an argument, but on webkitgtk, this requires converting the Storage object to its JS wrapper
importJS Unsafe "localStorage['getItem'](this[0])" "localStorageGetItemMaybe" [t| forall x m. MonadJS x m => String -> m (Maybe String) |]

askLocalStorage :: (HasWebView m, MonadIO m) => m Storage
askLocalStorage = do
  dw <- askDomWindow
  Just s <- liftIO $ getLocalStorage dw
  return s

type Key = String

type Value = String

storageGetInitial :: (HasWebView m, HasJS x m) => String -> m (Maybe Value)
storageGetInitial = liftM runIdentity . storageGetManyInitial . Identity

storageGetManyInitial :: (HasJS x m, Traversable f) => f String -> m (f (Maybe Value))
storageGetManyInitial keys = do
  liftJS $ forM keys localStorageGetItemMaybe

storageGet :: (MonadWidget t m, HasJS x (WidgetHost m)) => Event t (String) -> m (Event t (Maybe Value))
storageGet = liftM (fmap runIdentity) . storageGetMany . fmap Identity

storageGetMany :: (MonadWidget t m, HasJS x (WidgetHost m), Traversable f) => Event t (f String) -> m (Event t (f (Maybe Value)))
storageGetMany gets = do
  performEvent $ ffor gets $ liftJS . mapM localStorageGetItemMaybe

storageSet :: (PerformEvent t m, HasWebView m, MonadIO m, MonadIO (Performable m)) => Event t (String, Maybe Value) -> m (Event t ())
storageSet = liftM (fmap runIdentity) . storageSetMany . fmap Identity

storageSetMany :: (PerformEvent t m, HasWebView m, MonadIO m, MonadIO (Performable m), Traversable f) => Event t (f (String, Maybe Value)) -> m (Event t (f ()))
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

storageSetJson :: (MonadWidget t m, ToJSON a) => Event t (String, Maybe a) -> m (Event t ())
storageSetJson e = storageSet $ fmap (\(k, v) -> (k, fmap (T.unpack . T.decodeUtf8 . LBS.toStrict . encode) v)) e

storageGetJson :: (MonadWidget t m, FromJSON a, HasJS x (WidgetHost m)) => Event t String -> m (Event t (Maybe a))
storageGetJson e = do
  mv <- storageGet e
  return $ fmap (join . fmap (decodeValue' . LBS.fromStrict . T.encodeUtf8 . T.pack)) mv

storageGetJsonInitial :: (HasWebView m, HasJS x m, FromJSON a) => String -> m (Maybe a)
storageGetJsonInitial k = do
  mv <- storageGetInitial k
  return $ join $ fmap (decodeValue' . LBS.fromStrict . T.encodeUtf8 . T.pack) mv
