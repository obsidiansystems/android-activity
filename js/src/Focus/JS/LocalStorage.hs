{-# LANGUAGE GADTs, FlexibleContexts, TemplateHaskell, RankNTypes #-}
module Focus.JS.LocalStorage where

import Control.Monad
import Control.Monad.Identity
import Data.Aeson hiding (Value)
import Data.ByteString.Lazy as LBS
import Data.Text as T
import Data.Text.Encoding as T
import Focus.JS.Window
import Focus.Request
import Foreign.JavaScript.TH
import GHCJS.DOM.Types (MonadJSM)
import GHCJS.DOM.Storage
import GHCJS.DOM.Window (getLocalStorageUnchecked)
import Reflex.Dom hiding (Value)

askLocalStorage :: (HasJSContext m, MonadJSM m) => m Storage
askLocalStorage = do
  dw <- askDomWindow
  getLocalStorageUnchecked dw

type Key = String

type Value = String

storageGetInitial :: (HasJSContext m, MonadJSM m) => String -> m (Maybe Value)
storageGetInitial = liftM runIdentity . storageGetManyInitial . Identity

storageGetManyInitial :: (HasJSContext m, Traversable f, MonadJSM m) => f String -> m (f (Maybe Value))
storageGetManyInitial keys = do
  s <- askLocalStorage
  forM keys $ getItem s

storageGet :: MonadWidget t m => Event t (String) -> m (Event t (Maybe Value))
storageGet = liftM (fmap runIdentity) . storageGetMany . fmap Identity

storageGetMany :: (MonadWidget t m, Traversable f) => Event t (f String) -> m (Event t (f (Maybe Value)))
storageGetMany gets = do
  s <- askLocalStorage
  performEvent $ ffor gets $ mapM (getItem s)

storageSet :: (PerformEvent t m, HasJSContext m, MonadJSM m, MonadJSM (Performable m)) => Event t (String, Maybe Value) -> m (Event t ())
storageSet = liftM (fmap runIdentity) . storageSetMany . fmap Identity

storageSetMany :: (PerformEvent t m, HasJSContext m, MonadJSM m, MonadJSM (Performable m), Traversable f) => Event t (f (String, Maybe Value)) -> m (Event t (f ()))
storageSetMany sets = do
  dw <- askDomWindow
  s <- getLocalStorageUnchecked dw
  performEvent $ ffor sets $ mapM $ \(k, mv) -> case mv of
    Nothing -> removeItem s k
    Just v -> setItem s k v

storageRemoveAll :: MonadWidget t m => Event t () -> m ()
storageRemoveAll e = do
  dw <- askDomWindow
  s <- getLocalStorageUnchecked dw
  performEvent_ $ fmap (const $ clear s) e

storageSetJson :: (MonadWidget t m, ToJSON a) => Event t (String, Maybe a) -> m (Event t ())
storageSetJson e = storageSet $ fmap (\(k, v) -> (k, fmap (T.unpack . T.decodeUtf8 . LBS.toStrict . encode) v)) e

storageGetJson :: (MonadWidget t m, FromJSON a) => Event t String -> m (Event t (Maybe a))
storageGetJson e = do
  mv <- storageGet e
  return $ fmap (join . fmap (decodeValue' . LBS.fromStrict . T.encodeUtf8 . T.pack)) mv

storageGetJsonInitial :: (MonadJSM m, HasJSContext m, FromJSON a) => String -> m (Maybe a)
storageGetJsonInitial k = do
  mv <- storageGetInitial k
  return $ join $ fmap (decodeValue' . LBS.fromStrict . T.encodeUtf8 . T.pack) mv
