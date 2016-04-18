{-# LANGUAGE FlexibleContexts #-}
module Focus.JS.TextArea where

import Reflex.Dom
import GHCJS.DOM.HTMLTextAreaElement (HTMLTextAreaElement)
import qualified GHCJS.DOM.HTMLTextAreaElement as TA
import qualified GHCJS.DOM.Element as El

textAreaGetEnter :: Reflex t => TextArea t -> Event t ()
textAreaGetEnter i = fmapMaybe (\n -> if n == keycodeEnter then Just () else Nothing) $ _textArea_keypress i

attachSelectionStart :: (HasJS x (WidgetHost m), MonadWidget t m) => HTMLTextAreaElement -> Event t a -> m (Event t (Int, a))
attachSelectionStart t e = performEvent (fmap (\v -> do n <- TA.getSelectionStart t; return (n, v)) e)

setSelectionPos :: (HasJS x (WidgetHost m), MonadWidget t m) => HTMLTextAreaElement -> Event t Int -> m ()
setSelectionPos t e = performEvent_ (fmap (\n -> do TA.setSelectionStart t n; TA.setSelectionEnd t n; El.focus t) e)

