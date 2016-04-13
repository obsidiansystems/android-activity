{-# LANGUAGE TemplateHaskell #-}
module Focus.JS.EmojiOne ( module Focus.JS.EmojiOne
                         , module Focus.EmojiOne
                         ) where

import Reflex
import Reflex.Dom

import Focus.EmojiOne

emojiPicker :: MonadWidget t m => m (Event t String)
emojiPicker = return never
