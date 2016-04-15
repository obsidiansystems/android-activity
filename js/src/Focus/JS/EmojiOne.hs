{-# LANGUAGE TemplateHaskell #-}
module Focus.JS.EmojiOne ( module Focus.JS.EmojiOne
                         , module Focus.EmojiOne
                         ) where

import Data.Map (Map)
import Data.Monoid

import Reflex
import Reflex.Dom

import Focus.EmojiOne

emojiPicker :: MonadWidget t m => m (Event t String)
emojiPicker = return never

emojiAttrs :: EmojiData -> Map String String
emojiAttrs ed = mconcat
  [ "class" =: ("emojione emojione-" <> _emojiData_unicode ed)
  , "title" =: _emojiData_shortname ed
  ]

emojiEl :: MonadWidget t m => EmojiData -> m ()
emojiEl ed = elAttr "span" (emojiAttrs ed) blank

emojiElAttr :: MonadWidget t m => EmojiData -> Map String String -> m ()
emojiElAttr ed attrs' = elAttr "span" (emojiAttrs ed <> attrs') blank

emojiElDynAttr :: MonadWidget t m => EmojiData -> Dynamic t (Map String String) -> m ()
emojiElDynAttr ed attrs' = do
  attrs <- mapDyn (emojiAttrs ed <>) attrs'
  elDynAttr "span" attrs blank

emojiDynElAttrDyn :: MonadWidget t m => Dynamic t EmojiData -> Dynamic t (Map String String) -> m ()
emojiDynElAttrDyn edyn attrs' = do
  ad <- mapDyn emojiAttrs edyn
  attrs <- combineDyn (<>) ad attrs'
  elDynAttr "span" attrs blank
