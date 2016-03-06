{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}

module Focus.JS.FileReader (fileReader, blobFileReader, dataURLFileReader) where

import Reflex.Dom hiding (fromJSString)
import Control.Monad.Identity
import Reflex.Host.Class
import Data.Dependent.Sum
import Control.Monad.Trans
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.DOM.FileReader
import GHCJS.DOM.Types hiding (Event)
import GHCJS.DOM.EventM

-- Should askPostEvent/buildEvent be in Reflex.Dom or perhaps elsewhere in Focus?
-- They're useful for building up FFI stuff like this.
askPostEvent :: MonadWidget t m => m (EventTrigger t a -> a -> IO ())
askPostEvent = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  return (\t a -> postGui $ runWithActions [t :=> Identity a])

buildEvent :: (MonadWidget t m)
           => ((a -> IO ()) -> IO (IO ()))
           -> m (Event t a)
buildEvent install = do
  postEvent <- askPostEvent
  newEventWithTrigger (install . postEvent)

fileReader :: forall t m a. (MonadWidget t m)
           => (FileReader -> Maybe File -> WidgetHost m ()) -- There are a bunch of functions of this shape in GHCJS.DOM.FileReader
           -> (JSVal -> IO (Maybe a))
           -> Event t File -> m (Event t a)
fileReader readAs processResult request =
  do fr <- liftIO newFileReader
     let handler :: (Maybe a -> IO ()) -> EventM FileReader UIEvent ()
         handler k = liftIO $ k =<< (processResult =<< getResult fr)
     performEvent_ (fmap (\f -> readAs fr (Just f)) request)
     e <- buildEvent (on fr load . handler)
     return (fmapMaybe id e)
        -- For ordinary use cases, there shouldn't actually be Nothing firings as far as I can tell
        -- It's just that fromJSVal produces a Maybe result for obvious dynamic typing reasons.

blobFileReader :: (MonadWidget t m) => Event t File -> m (Event t Blob)
blobFileReader = fileReader readAsArrayBuffer fromJSVal

dataURLFileReader :: (MonadWidget t m) => Event t File -> m (Event t String)
dataURLFileReader = fileReader readAsDataURL (fmap (fmap fromJSString) . fromJSVal)

