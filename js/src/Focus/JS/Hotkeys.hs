{-# LANGUAGE LambdaCase, ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module Focus.JS.Hotkeys where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import GHCJS.DOM.Types (IsElement, KeyboardEvent)
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.EventM as E
import qualified GHCJS.DOM.HTMLElement as HE
import qualified GHCJS.DOM.KeyboardEvent as E
import Reflex.Dom

data KeyModifier = KeyModifier_Alt
                 | KeyModifier_Ctrl
                 | KeyModifier_Meta
                 | KeyModifier_Shift
                 | KeyModifier_AltGr
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Bind keycodes + modifiers
bindKeys :: (MonadIO m, MonadWidget t m, IsElement e)
         => E.EventName e E.KeyboardEvent -- ^ The keyboard event name (e.g., keyDown)
         -> e -- ^ The element to listen on
         -> Map (Int, Set KeyModifier) (ReaderT  E.KeyboardEvent IO k) -- ^ Map of key combinations to actions
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
inputTarget :: ReaderT KeyboardEvent IO Bool
inputTarget = do
  Just e <- fmap (fmap HE.castToHTMLElement) E.eventTarget
  n :: Maybe String <- E.getTagName e
  return $ n `elem` (map Just ["INPUT", "SELECT", "TEXTAREA"])
