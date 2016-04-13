{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Focus.JS.EmojiOne ( module Focus.JS.EmojiOne
                         , module Focus.EmojiOne
                         ) where

import Control.Lens
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Reflex
import Reflex.Dom

import Focus.AppendMap
import Focus.EmojiOne

emojisByCategory :: Map String (Map Int String)
emojisByCategory = emojiData
        & ifoldMap (\sn ed -> AppendMap $ Map.singleton (ed ^. emojiData_category) (Map.singleton (ed ^. emojiData_emojiOrder) sn))
        & view _Wrapped

emojiCategories :: [String]
emojiCategories = Map.keys emojisByCategory

supportedCategories :: [String]
supportedCategories =
  [ "people"
  , "nature"
  , "activity"
  , "food"
  , "objects"
  , "travel"
  , "flags"
  , "symbols"
  ]

emojiPicker :: (MonadWidget t m) => Map String FilePath -> m (Event t String)
emojiPicker assets = do
  let emojisBySupportedCategory = Map.intersection emojisByCategory (Map.fromSet (\_ -> ()) $ Set.fromList supportedCategories)
  selE <- el "div" $ fmap leftmost $ forM supportedCategories $ \c -> do
    Just path <- return $ do
      (sn,_) <- Map.minView =<< Map.lookup c emojisBySupportedCategory
      Map.lookup sn assets
    --TODO: style selected element
    e <- emojiEl path c
    return $ c <$ domEvent Click e
  selDyn <- holdDyn (head supportedCategories) selE
  let emojiDyn = constDyn emojisBySupportedCategory
  fmap (fmap snd) $ selectViewListWithKey selDyn emojiDyn $ \_ emojis' isSel' -> do
    pb <- getPostBuild
    fmap switchPromptlyDyn $ widgetHold (return never) $
      ffor (attachDyn isSel' $ leftmost [tagDyn emojis' pb, updated emojis']) $ \(isSel, emojis) -> case isSel of
        False -> return never
        True -> fmap leftmost $ forM (map snd $ Map.toAscList emojis) $ \sn -> do
          Just path <- return $ Map.lookup sn assets
          e <- emojiEl path (mconcat [":", sn, ":"])
          return $ sn <$ domEvent Click e
 where
  emojiEl path alt = fmap fst $
    el' "span" $ elAttr "img" (mconcat
      [ "class" =: "emojione"
      , "src" =: path
      , "type" =: "image/svg+xml"
      , "alt" =: alt
      ]) blank

