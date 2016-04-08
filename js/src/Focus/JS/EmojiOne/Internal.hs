{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Focus.JS.EmojiOne.Internal where

import Control.Monad
import Control.Lens
import Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import System.FilePath

import Focus.HTTP.TH
import Focus.TH

data EmojiRaw = EmojiRaw
       { _emojiRaw_unicode :: String
       , _emojiRaw_unicode_alternates :: String
       , _emojiRaw_name :: String
       , _emojiRaw_shortname :: String --NB: shortname is how emojione spells it, no camelCase
       , _emojiRaw_category :: String
       , _emojiRaw_emoji_order :: String
       , _emojiRaw_aliases :: [String]
       , _emojiRaw_aliases_ascii :: [String]
       , _emojiRaw_keywords :: [String]
       }
  deriving (Eq, Ord, Show, Read)

data EmojiData = EmojiData { _emojiData_unicode :: String
                           }
  deriving (Eq, Ord, Show, Read)

{-
TODO: Determine how much of this metadata we care about
data EmojiData = EmojiData { _emojiData_unicode :: String
                           , _emojiData_unicodeAlternates :: String
                           , _emojiData_name :: String
                           , _emojiData_shortname :: String
                           , _emojiData_category :: String
                           , _emojiData_emojiOrder :: Int
                           , _emojiData_aliases :: [String]
                           , _emojiData_aliasesAscii :: [String]
                           , _emojiData_keywords :: [String]
                           }
  deriving (Eq, Ord, Show, Read)
-}

emojiData' :: Q Exp
emojiData' = do
  Right (d :: Map String EmojiRaw) <- eitherDecode . LB.fromStrict <$> qReadFile "emojione/emoji.json"
  let d' = Map.toList $ flip imap d $ \_ eraw -> EmojiData
        { _emojiData_unicode = _emojiRaw_unicode eraw
        }
        {-
        , _emojiData_unicodeAlternates = _emojiRaw_unicode_alternates eraw
        , _emojiData_name = _emojiRaw_name eraw
        , _emojiData_shortname = _emojiRaw_shortname eraw
        , _emojiData_category = _emojiRaw_category eraw
        , _emojiData_emojiOrder = read (_emojiRaw_emoji_order eraw)
        , _emojiData_aliases = _emojiRaw_aliases eraw
        , _emojiData_aliasesAscii = _emojiRaw_aliases_ascii eraw
        , _emojiData_keywords = _emojiRaw_keywords eraw
        } -}
  ((VarE 'Map.fromList) `AppE`) <$> lift d'

mkEmojiAssetPaths :: FilePath -> Map String EmojiData -> Q Exp
mkEmojiAssetPaths root eds = do
  epaths <- forM eds $ \ed -> assetPath' root ("emojione/assets/svg" </> (_emojiData_unicode ed <> ".svg"))
  ((VarE 'Map.fromList) `AppE`) <$> lift (Map.toList epaths)

$(A.deriveJSON A.defaultOptions{ A.fieldLabelModifier = drop (length "_emojiRaw_") } ''EmojiRaw)

--TODO: GHC 8 will have DeriveLift
instance Lift EmojiData where
  lift ed = recConE 'EmojiData $ [ (,) '_emojiData_unicode <$> lift (_emojiData_unicode ed) ]

{-
instance Lift EmojiData where
  lift ed = recConE 'EmojiData $ 
              [ (,) '_emojiData_unicode <$> lift (_emojiData_unicode ed)
              , (,) '_emojiData_unicodeAlternates <$> lift (_emojiData_unicodeAlternates ed)
              , (,) '_emojiData_name <$> lift (_emojiData_name ed)
              , (,) '_emojiData_shortname <$> lift (_emojiData_shortname ed)
              , (,) '_emojiData_category <$> lift (_emojiData_category ed)
              , (,) '_emojiData_emojiOrder <$> lift (_emojiData_emojiOrder ed)
              , (,) '_emojiData_aliases <$> lift (_emojiData_aliases ed)
              , (,) '_emojiData_aliasesAscii <$> lift (_emojiData_aliasesAscii ed)
              , (,) '_emojiData_keywords <$> lift (_emojiData_keywords ed)
              ]
-}
