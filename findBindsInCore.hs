{-# LANGUAGE OverloadedStrings #-}
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment

main :: IO ()
main = do
  x <- T.getContents
  T.putStr $ T.unlines $ catMaybes $ fmap (\x -> let u = T.unlines x in if " >>" `T.isInfixOf` u || "(>>" `T.isInfixOf` u then listToMaybe $ drop 1 x else Nothing) $ splitOn [""] $ T.lines x
