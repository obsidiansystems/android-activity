{-# LANGUAGE LambdaCase #-}
module Focus.Highlight where

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char

type HighlightedText = [Highlight]

data Highlight = Highlight_On Text
               | Highlight_Off Text
  deriving (Show, Read, Eq, Ord)

unHighlight :: Highlight -> Text
unHighlight = \case
  Highlight_On t -> t
  Highlight_Off t -> t

toText :: HighlightedText -> Text
toText = foldl (<>) T.empty . map unHighlight

isHighlighted :: Highlight -> Bool
isHighlighted = \case
  Highlight_On _ -> True
  _ -> False

nonEmpty :: Text -> Maybe Text
nonEmpty = (\x -> if T.null x then Nothing else Just x) . T.strip

nonNull :: T.Text -> Maybe Text
nonNull x = if T.null x then Nothing else Just x

highlight :: (Text -> Text) -- ^ Text transform for query and text, e.g. case fold
          -> Text -- ^ Query text
          -> Text -- ^ Text to search within
          -> HighlightedText
highlight transform query t = case fmap transform $ nonEmpty query of
  Nothing -> [Highlight_Off t]
  Just q -> case T.breakOnAll q (transform t) of
    [] -> [Highlight_Off t]
    ms -> let l = T.length q
          in (\(x, _, r) -> x <> maybe [] ((:[]) . Highlight_Off) (nonNull r)) $ foldl (\(hs, cursor, t') (prefix, _) -> 
              let (off, match') = T.splitAt (T.length (T.drop cursor prefix)) t'
                  (h, rest) = T.splitAt l match'
                  newHighlights = maybe [Highlight_On h] (\x -> [Highlight_Off x, Highlight_On h]) (nonNull off)
              in (hs <> newHighlights , cursor + (T.length off + l), rest)) ([], 0, t) ms

highlightCaseSensitive :: Text -> Text -> HighlightedText
highlightCaseSensitive = highlight id

highlightCaseInsensitive :: Text -> Text -> HighlightedText
highlightCaseInsensitive = highlight T.toCaseFold

highlightPrefix :: (Text -> Text)
                -> Text
                -> Text
                -> HighlightedText
highlightPrefix transform query t = case fmap (T.words . transform) $ nonEmpty query of
  Nothing -> [Highlight_Off t]
  Just qs ->
    let sortedQs = reverse $ sortOn T.length qs
    in mergeHighlights $ flip concatMap (wordsWithWhitespace t) $ \w ->
        let w' = transform w
        in case listToMaybe (filter (\q -> q `T.isPrefixOf` w') sortedQs) of
                Just n -> highlightSelection w 0 (T.length n)
                Nothing -> [Highlight_Off w]
         -- then let (match, rest) = T.splitAt (T.length q) w
         --      in [Highlight_On match, Highlight_Off rest]
         -- else [Highlight_Off w]

highlightSelection :: Text
                   -> Int
                   -> Int
                   -> HighlightedText
highlightSelection t start num = 
  let (off, rest) = T.splitAt start t
      (on, rest') = T.splitAt num rest
  in [Highlight_Off off, Highlight_On on, Highlight_Off rest']

mergeHighlights :: HighlightedText -> HighlightedText
mergeHighlights hs = case hs of
  (x:y:xs) -> case (x, y) of
    (Highlight_On a, Highlight_On b) -> mergeHighlights (ifNonEmpty a b Highlight_On <> xs)
    (Highlight_Off a, Highlight_Off b) -> mergeHighlights (ifNonEmpty a b Highlight_Off <> xs)
    _ -> if T.null (unHighlight x)
            then mergeHighlights (y:xs)
            else x : mergeHighlights (y:xs)
  _ -> hs
  where
    ifNonEmpty a b f = let ab = a <> b in if T.null ab then [] else [f ab]

-- Like T.words, but doesn't delete the whitespaces (they count as separate words)
wordsWithWhitespace :: Text -> [Text]
wordsWithWhitespace t =
  let (pre, rest) = T.break isSpace t
      ws = T.takeWhile isSpace rest
      rest' = T.drop (T.length ws) rest
  in (pre : if T.null ws then [] else [ws]) ++ if T.null rest' then [] else wordsWithWhitespace rest'

