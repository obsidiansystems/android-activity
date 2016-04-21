{-# LANGUAGE RecursiveDo, TemplateHaskell, TypeFamilies #-}

module Focus.JS.AltInput where

import Reflex.Host.Class
import Reflex.Dom hiding (TextArea(..), textArea, TextAreaConfig(..),
                          TextInput(..), textInput, TextInputConfig(..), textInputGetEnter)
import Data.Default
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity
import qualified Data.Map as Map
import Data.Map (Map)

import GHCJS.DOM.HTMLInputElement as Input
import GHCJS.DOM.HTMLTextAreaElement as TextArea
import GHCJS.DOM.EventM
import GHCJS.DOM.Element hiding (error)
import Data.Maybe
import Data.Dependent.Sum (DSum (..))

data TextInput t
   = TextInput { _textInput_value :: Dynamic t String
               , _textInput_input :: Event t String
               , _textInput_keypress :: Event t Int
               , _textInput_keydown :: Event t Int
               , _textInput_keyup :: Event t Int
               , _textInput_hasFocus :: Dynamic t Bool
               , _textInput_element :: HTMLInputElement
               }

data TextInputConfig t
   = TextInputConfig { _textInputConfig_inputType :: String
                     , _textInputConfig_initialValue :: String
                     , _textInputConfig_setValue :: Event t String
                     , _textInputConfig_attributes :: Dynamic t (Map String String)
                     }

instance Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig { _textInputConfig_inputType = "text"
                        , _textInputConfig_initialValue = ""
                        , _textInputConfig_setValue = never
                        , _textInputConfig_attributes = constDyn mempty
                        }

-- | Create an input whose value is a string.  By default, the "type" attribute is set to "text", but it can be changed using the _textInputConfig_inputType field.  Note that only types for which the value is always a string will work - types whose value may be null will not work properly with this widget.
textInput :: MonadWidget t m => TextInputConfig t -> m (TextInput t)
textInput (TextInputConfig inputType initial eSetValue dAttrs) = do
  e <- liftM castToHTMLInputElement $ buildEmptyElement "input" =<< mapDyn (Map.insert "type" inputType) dAttrs
  Input.setValue e $ Just initial
  eSetValue' <- performEvent $ fmap (\v -> do Input.setValue e (Just v); return v) eSetValue
  eChange <- wrapDomEvent e (`on` input) $ fromMaybe "" <$> Input.getValue e
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  eChangeFocus <- newEventWithTrigger $ \eChangeFocusTrigger -> do
    unsubscribeOnblur <- on e blurEvent $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> Identity False]
    unsubscribeOnfocus <- on e focusEvent $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> Identity True]
    return $ liftIO $ unsubscribeOnblur >> unsubscribeOnfocus
  dFocus <- holdDyn False eChangeFocus
  eKeypress <- wrapDomEvent e (`on` keyPress) getKeyEvent
  eKeydown <- wrapDomEvent e (`on` keyDown) getKeyEvent
  eKeyup <- wrapDomEvent e (`on` keyUp) getKeyEvent
  dValue <- holdDyn initial $ leftmost [eSetValue', eChange]
  return $ TextInput dValue eChange eKeypress eKeydown eKeyup dFocus e

textInputGetEnter :: Reflex t => TextInput t -> Event t ()
textInputGetEnter i = fmapMaybe (\n -> if n == keycodeEnter then Just () else Nothing) $ _textInput_keypress i

data TextArea t
   = TextArea { _textArea_value :: Dynamic t String
              , _textArea_input :: Event t String
              , _textArea_keypress :: Event t Int
              , _textArea_keydown :: Event t Int
              , _textArea_keyup :: Event t Int
              , _textArea_hasFocus :: Dynamic t Bool
              , _textArea_element :: HTMLTextAreaElement
              }

data TextAreaConfig t
   = TextAreaConfig { _textAreaConfig_initialValue :: String
                    , _textAreaConfig_setValue :: Event t String
                    , _textAreaConfig_attributes :: Dynamic t (Map String String)
                    }

instance Reflex t => Default (TextAreaConfig t) where
  def = TextAreaConfig { _textAreaConfig_initialValue = ""
                       , _textAreaConfig_setValue = never
                       , _textAreaConfig_attributes = constDyn mempty
                       }

instance HasValue (TextArea t) where
  type Value (TextArea t) = Dynamic t String
  value = _textArea_value

textArea :: MonadWidget t m => TextAreaConfig t -> m (TextArea t)
textArea (TextAreaConfig initial eSetValue dAttrs) = do
  e <- liftM castToHTMLTextAreaElement $ buildEmptyElement "textarea" dAttrs
  TextArea.setValue e $ Just initial
  eSetValue' <- performEvent $ fmap (\v -> do TextArea.setValue e (Just v); return v) eSetValue
  eChange <- wrapDomEvent e (`on` input) $ fromMaybe "" <$> TextArea.getValue e
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  eChangeFocus <- newEventWithTrigger $ \eChangeFocusTrigger -> do
    unsubscribeOnblur <- on e blurEvent $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> Identity False]
    unsubscribeOnfocus <- on e focusEvent $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> Identity True]
    return $ liftIO $ unsubscribeOnblur >> unsubscribeOnfocus
  dFocus <- holdDyn False eChangeFocus
  eKeypress <- wrapDomEvent e (`on` keyPress) getKeyEvent
  eKeydown <- wrapDomEvent e (`on` keyDown) getKeyEvent
  eKeyup <- wrapDomEvent e (`on` keyUp) getKeyEvent
  dValue <- holdDyn initial $ leftmost [eSetValue', eChange]
  return $ TextArea dValue eChange eKeypress eKeydown eKeyup dFocus e

textAreaGetEnter :: Reflex t => TextArea t -> Event t ()
textAreaGetEnter i = fmapMaybe (\n -> if n == keycodeEnter then Just () else Nothing) $ _textArea_keypress i
