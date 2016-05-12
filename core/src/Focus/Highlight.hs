{-# LANGUAGE LambdaCase #-}
module Focus.Highlight where

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

type HighlightedText = [Highlight]

data Highlight = Highlight_On Text
               | Highlight_Off Text
  deriving (Show, Read, Eq, Ord)

unHighlight :: Highlight -> Text
unHighlight = \case
  Highlight_On t -> t
  Highlight_Off t -> t

toText :: HighlightedText -> Text
toText = T.unlines . map unHighlight

isHighlighted :: Highlight -> Bool
isHighlighted = \case
  Highlight_On _ -> True
  _ -> False

nonEmpty :: Text -> Maybe Text
nonEmpty = (\x -> if T.null x then Nothing else Just x) . T.strip

highlight :: (Text -> Text) -- ^ Text transform for query and text, e.g. case fold
          -> Text -- ^ Query text
          -> Text -- ^ Text to search within
          -> HighlightedText
highlight transform query t = case fmap transform $ nonEmpty query of
  Nothing -> [Highlight_Off t]
  Just q -> case T.breakOnAll q (transform t) of
    [] -> [Highlight_Off t]
    ms -> let l = T.length q
          in (\(x, _, r) -> x <> maybe [] ((:[]) . Highlight_Off) (nonEmpty r)) $ foldl (\(hs, cursor, t') (prefix, _) -> 
              let (off, match') = T.splitAt (T.length (T.drop cursor prefix)) t'
                  (h, rest) = T.splitAt l match'
                  newHighlights = maybe [Highlight_On h] (\x -> [Highlight_Off x, Highlight_On h]) (nonEmpty off)
              in (hs <> newHighlights , cursor + (T.length off + l), rest)) ([], 0, t) ms

highlightCaseSensitive :: Text -> Text -> HighlightedText
highlightCaseSensitive = highlight id

highlightCaseInsensitive :: Text -> Text -> HighlightedText
highlightCaseInsensitive = highlight T.toCaseFold

