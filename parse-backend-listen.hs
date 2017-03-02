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
    let acc :: (Map UTCTime ([Text], Int), Map UTCTime ([Text], Int)) -> [Text] -> (Map UTCTime ([Text], Int), Map UTCTime ([Text], Int))
        acc um@(unmatchedOpens, unmatchedCloses) (header:rawtime:rest)
          | Just time <- parseTime rawtime
          , header == "RESPONSE" = case Map.lookup time unmatchedOpens of
              Nothing -> case Map.lookup time unmatchedCloses of
                Just (x, count) -> (unmatchedOpens, Map.insert time (x, count+1) unmatchedCloses)
                Nothing -> (unmatchedOpens, Map.insert time (rest, 1) unmatchedCloses)
              Just (x, count) | count == 1 -> (Map.delete time unmatchedOpens, unmatchedCloses)
                              | count > 1 -> (Map.insert time (x, count-1) unmatchedOpens, unmatchedCloses)
                              | otherwise -> error "request count invariant failed"
          | Just time <- parseTime rawtime
          , header == "REQUEST" = case Map.lookup time unmatchedOpens of
              Nothing -> (Map.insert time (rest, 1) unmatchedOpens, unmatchedCloses)
              Just (x, count) -> (Map.insert time (x, count+1) unmatchedOpens, unmatchedCloses)
          | otherwise = um
         where parseTime = parseTimeM False defaultTimeLocale tsFormat . T.unpack
        acc um _ = um
        pipeline :: Text -> (Map UTCTime ([Text], Int), Map UTCTime ([Text], Int))
        pipeline = foldl' acc (Map.empty, Map.empty) . map (T.splitOn "\t") . T.lines
        (unmatchedRequests, unmatchedResponses) = pipeline contents
    T.putStrLn "### Unmatched Requests"
    forM_ (Map.toList unmatchedRequests) $ \(k, (x,count)) -> do
      T.putStrLn . T.intercalate "\t" $
        ["REQUEST", T.pack (formatTime defaultTimeLocale tsFormat k)] ++ x
      when (count > 1) $ putStrLn $ mconcat $
        ["\t... and ", show (count - 1), " unmatched request(s) at the same time."]
    T.putStrLn "### Unmatched Responses"
    forM_ (Map.toList unmatchedResponses) $ \(k, (x, count)) -> do
      T.putStrLn . T.intercalate "\t" $
        ["RESPONSE", T.pack (formatTime defaultTimeLocale tsFormat k)] ++ x
      when (count > 1) $ putStrLn $ mconcat $
        ["\t... and ", show (count - 1), " unmatched response(s) at the same time."]
