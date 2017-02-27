#! /usr/bin/env nix-shell
#! nix-shell "script-env/parse-backend-listen.nix" -i "runghc"
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.IO as TS
import Data.Time (UTCTime)
import Data.Time.Format
import System.Environment

import Debug.Trace

tsFormat :: String
tsFormat = iso8601DateFormat (Just "%H:%M:%SZ")

main :: IO ()
main = do
  files <- getArgs
  forM_ files $ \file -> do
    contents <- T.fromStrict <$> TS.readFile file
    let acc :: (Map UTCTime [Text], Map UTCTime [Text]) -> [Text] -> (Map UTCTime [Text], Map UTCTime [Text])
        acc um@(unmatchedOpens, unmatchedCloses) (header:rawtime:rest)
          | header == "RESPONSE" = case Map.lookup time unmatchedOpens of
              Nothing -> (unmatchedOpens, Map.insert time rest unmatchedCloses)
              Just _ -> (Map.delete time unmatchedOpens, unmatchedCloses)
          | header == "REQUEST" = (Map.insert time rest unmatchedOpens, unmatchedCloses)
          | otherwise = um
         where Just time = parseTimeM False defaultTimeLocale tsFormat (T.unpack rawtime)
        acc acc _ = acc
        pipeline :: Text -> (Map UTCTime [Text], Map UTCTime [Text])
        pipeline = foldl' acc (Map.empty, Map.empty) . map (T.splitOn "\t") . T.lines
        (unmatchedRequests, unmatchedResponses) = pipeline contents
    T.putStrLn "### Unmatched Requests"
    forM_ (Map.toList unmatchedRequests) $ \(k, x) -> T.putStrLn . T.intercalate "\t" $
      ["REQUEST", T.pack (formatTime defaultTimeLocale tsFormat k)] ++ x
    T.putStrLn "### Unmatched Responses"
    forM_ (Map.toList unmatchedResponses) $ \(k, x) -> T.putStrLn . T.intercalate "\t" $
      ["RESPONSE", T.pack (formatTime defaultTimeLocale tsFormat k)] ++ x
