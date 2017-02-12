{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Focus.JS.TextArea where

import Reflex.Dom.Core
import GHCJS.DOM.HTMLTextAreaElement (HTMLTextAreaElement)
import qualified GHCJS.DOM.HTMLTextAreaElement as TA
import qualified GHCJS.DOM.Element as El
import GHCJS.DOM.Types (MonadJSM)

textAreaGetEnter :: Reflex t => TextArea t -> Event t ()
textAreaGetEnter i = fmapMaybe (\n -> if keyCodeLookup n == Enter then Just () else Nothing) $ _textArea_keypress i

attachSelectionStart :: (HasJS x (WidgetHost m), PerformEvent t m, MonadJSM (Performable m)) => HTMLTextAreaElement -> Event t a -> m (Event t (Int, a))
attachSelectionStart t e = performEvent (fmap (\v -> do n <- TA.getSelectionStart t; return (n, v)) e)

setSelectionPos :: (HasJS x (WidgetHost m), PerformEvent t m, MonadJSM (Performable m)) => HTMLTextAreaElement -> Event t Int -> m ()
setSelectionPos t e = performEvent_ (fmap (\n -> do TA.setSelectionStart t n; TA.setSelectionEnd t n; El.focus t) e)

