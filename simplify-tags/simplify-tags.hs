{-# LANGUAGE OverloadedStrings #-}

-- Script to remove redundant tags file entries (those for the same identifier which are within 50 lines of each other in
-- the same source file).

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.List

data Tag = Tag { name, file :: Text, line :: Integer }
  deriving (Eq, Ord)

extractTags s = sort [Tag n f l | [n, f, lT] <- map (T.splitOn "\t") . T.lines $ s, (l, "") <- reads (T.unpack lT)]

renderTag t = T.intercalate "\t" [name t, file t, T.pack (show (line t))]

filterTags = map head . groupBy (\t t' -> name t == name t' && file t == file t' && abs (line t - line t') < 50)

main = do
  ts <- extractTags <$> T.readFile "tags"
  let ts' = filterTags ts
  putStrLn $ "simplify-tags: Culled " ++ show (length ts - length ts') ++ " redundant tags."
  T.writeFile "tags" . T.unlines . map renderTag $ ts'
  
