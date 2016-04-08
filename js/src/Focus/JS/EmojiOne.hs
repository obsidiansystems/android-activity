{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O0 #-}
module Focus.JS.EmojiOne ( module Focus.JS.EmojiOne
                         , mkEmojiAssetPaths
                         ) where

import Data.Map (Map)

import Focus.JS.EmojiOne.Internal

emojiData :: Map String EmojiData
emojiData = $emojiData'
