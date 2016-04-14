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
  let submap k = flip imap emojisBySupportedCategory $ \k' m -> if k' == k
        then Just m
        else Nothing
      selectList selection' mkChild = do
        pb <- getPostBuild
        selection <- mapDyn submap selection'
        liftM switchPromptlyDyn $ widgetHold (return never) $ ffor (fmap (Map.mapMaybe id) $ tagDyn selection pb) $ \sel0 -> do
          selectChild <- listHoldWithKey sel0 (updated selection) $ \k v -> do
            selected <- mapDyn (==k) selection'
            selectSelf <- mkChild k v selected
            return $ fmap ((,) k) selectSelf
          liftM switchPromptlyDyn $ mapDyn (leftmost . Map.elems) selectChild
  fmap (fmap snd) $ selectList selDyn $ \_ emojis isSel' -> do
    pb <- getPostBuild
    fmap switchPromptlyDyn $ widgetHold (return never) $
      ffor (tagDyn isSel' pb) $ \case
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


