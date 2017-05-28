{-# LANGUAGE LambdaCase, ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module Focus.JS.Hotkeys where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import GHCJS.DOM.Types
       (JSM, IsElement, KeyboardEvent, MonadJSM,
        uncheckedCastTo, HTMLElement(..), EventTarget(..))
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.EventM as E
import qualified GHCJS.DOM.KeyboardEvent as E
import Reflex.Dom.Core

data KeyModifier = KeyModifier_Alt
                 | KeyModifier_Ctrl
                 | KeyModifier_Meta
                 | KeyModifier_Shift
                 | KeyModifier_AltGr
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Bind keycodes + modifiers
bindKeys :: (MonadJSM m, MonadWidget t m, IsElement e)
         => E.EventName e E.KeyboardEvent -- ^ The keyboard event name (e.g., keyDown)
         -> e -- ^ The element to listen on
         -> Map (Word, Set KeyModifier) (ReaderT  E.KeyboardEvent JSM k) -- ^ Map of key combinations to actions
         -> m (Event t k) -- ^ Event that fires when any of the specified key combinations is pressed
bindKeys keyEv keyTarget hotkeys = do
  fmap (fmapMaybe id) $ wrapDomEvent keyTarget (`E.on` keyEv) $ do
    e <- E.event
    keyEvent <- getKeyEvent
    activeMods <- fmap (Map.keysSet . Map.filter id . Map.fromList . zip [minBound .. maxBound]) $
      forM [minBound .. maxBound] $ \case
        KeyModifier_Alt -> E.getAltKey e
        KeyModifier_Ctrl -> E.getCtrlKey e
        KeyModifier_Meta -> E.getMetaKey e
        KeyModifier_Shift -> E.getShiftKey e
        KeyModifier_AltGr -> E.getAltGraphKey e
    let activeHotkey = listToMaybe $ Map.elems $
          Map.filterWithKey (\(kc, reqMods) _ -> keyEvent == kc && reqMods == activeMods) hotkeys
    sequence activeHotkey

-- | Determine whether the event target is an element that takes input
inputTarget :: ReaderT KeyboardEvent JSM Bool
inputTarget = do
  Just e <- fmap (uncheckedCastTo HTMLElement) <$> E.eventTarget
  n :: String <- E.getTagName e
  return $ n `elem` ["INPUT", "SELECT", "TEXTAREA"]
