{-# LANGUAGE TemplateHaskell, LambdaCase, RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings #-}
module Focus.JS.EmojiOne ( module Focus.JS.EmojiOne
                         , module Focus.EmojiOne
                         ) where

import Control.Lens
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set
import Reflex
import Reflex.Dom
import Data.Text (Text)

import Focus.AppendMap
import Focus.EmojiOne

emojiAttrs :: EmojiData -> Map Text Text -> Map Text Text
emojiAttrs ed attrs = attrs
  & (at "class" %~ \x -> Just (maybe "" id x <> " emojione emojione-" <> _emojiData_unicode ed))
  & (at "title" %~ \case
      Nothing -> Just (_emojiData_shortname ed)
      Just x -> Just x)
  & (at "style" %~ \x -> Just (maybe "" id x <> " zoom: calc(1/3); "))

emojiEl :: MonadWidget t m => EmojiData -> m (El t)
emojiEl ed = fst <$> elAttr' "span" (emojiAttrs ed Map.empty) blank

emojiElAttr :: MonadWidget t m => EmojiData -> Map Text Text -> m (El t)
emojiElAttr ed attrs' = fst <$> elAttr' "span" (emojiAttrs ed attrs') blank

emojiElDynAttr :: MonadWidget t m => EmojiData -> Dynamic t (Map Text Text) -> m (El t)
emojiElDynAttr ed attrs' = do
  attrs <- mapDyn (emojiAttrs ed) attrs'
  fst <$> elDynAttr' "span" attrs blank

emojiDynElAttrDyn :: MonadWidget t m => Dynamic t EmojiData -> Dynamic t (Map Text Text) -> m (El t)
emojiDynElAttrDyn edyn attrs' = do
  ad <- mapDyn (flip emojiAttrs Map.empty) edyn
  attrs <- combineDyn (<>) ad attrs'
  fst <$> elDynAttr' "span" attrs blank

emojisByCategory :: Map Text (Map Int Text)
emojisByCategory = emojiData
        & ifoldMap (\sn ed -> AppendMap $ Map.singleton (ed ^. emojiData_category) (Map.singleton (ed ^. emojiData_emojiOrder) sn))
        & view _Wrapped

emojiCategories :: [Text]
emojiCategories = Map.keys emojisByCategory

supportedCategories :: [Text]
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

emojiPicker :: MonadWidget t m => m (Event t Text)
emojiPicker = do
  let emojisBySupportedCategory = Map.intersection emojisByCategory (Map.fromSet (\_ -> ()) $ Set.fromList supportedCategories)
  rec selE <- divClass "emoji-picker-categories" $ el "span" $ fmap leftmost $ forM supportedCategories $ \c -> do
        Just ed <- return $ do
          (sn,_) <- Map.minView =<< Map.lookup c emojisBySupportedCategory
          Map.lookup sn emojiData
        --TODO: style selected element
        attr <- forDyn selDyn $ \c' -> "style" =: (mconcat
          [ if c == c'
              then "background-color: #aaccff; "
              else ""
          , "border: 1px solid #6688aa; "
          ])
        e <- emojiElDynAttr ed attr
        return $ c <$ domEvent Click e
      selDyn <- holdDyn (head supportedCategories) selE
  let submap k = flip imap emojisBySupportedCategory $ \k' m -> if k' == k
        then Just m
        else Nothing
      selectList selection' mkChild = do
        pb <- getPostBuild
        selection <- mapDyn submap (nubDyn selection')
        liftM switchPromptlyDyn $ widgetHold (return never) $ ffor (fmap (Map.mapMaybe id) $ tagDyn selection pb) $ \sel0 -> do
          selectChild <- listHoldWithKey sel0 (updated selection) $ \k v -> do
            selected <- mapDyn (==k) selection'
            selectSelf <- mkChild k v selected
            return $ fmap ((,) k) selectSelf
          liftM switchPromptlyDyn $ mapDyn (leftmost . Map.elems) selectChild
  fmap (fmap snd) $ divClass "emoji-picker-options" $ selectList selDyn $ \_ emojis isSel' -> do
    pb <- getPostBuild
    fmap switchPromptlyDyn $ widgetHold (return never) $
      ffor (tagDyn isSel' pb) $ \case
        False -> return never
        True -> fmap leftmost $ forM (map snd $ Map.toAscList emojis) $ \sn -> do
          Just ed <- return $ Map.lookup sn emojiData
          e <- emojiEl ed
          return $ sn <$ domEvent Click e
