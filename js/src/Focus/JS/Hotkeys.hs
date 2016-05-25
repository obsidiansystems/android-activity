{-# LANGUAGE LambdaCase #-}
module Focus.JS.Hotkeys where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import GHCJS.DOM.Types (IsElement)
import qualified GHCJS.DOM.EventM as E
import qualified GHCJS.DOM.KeyboardEvent as E
import Reflex.Dom

data KeyModifier = KeyModifier_Alt
                 | KeyModifier_Ctrl
                 | KeyModifier_Meta
                 | KeyModifier_Shift
                 | KeyModifier_AltGr
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

bindKeys :: (MonadIO m, MonadWidget t m, IsElement e)
         => E.EventName e E.KeyboardEvent
         -> e
         -> Map (Int, Set KeyModifier) (ReaderT E.KeyboardEvent IO k)
         -> m (Event t k)
bindKeys keyEv element hotkeys = do
  fmap (fmapMaybe id) $ wrapDomEvent element (`E.on` keyEv) $ do
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

