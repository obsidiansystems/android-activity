module Focus.Time.Zone where

import qualified Data.Set as Set
import System.Directory.Tree

getTimezoneNames :: FilePath -> IO [String]
getTimezoneNames dir = do
  tree <- build dir
  let filenames = concatMap getFilenames $ contents $ dirTree tree
      excludedFiles = Set.fromList ["zone.tab", "posixrules", "iso3166.tab"]
  return $ filter (not . flip Set.member excludedFiles) filenames
  where
    getFilenames :: DirTree t -> [FileName]
    getFilenames x = case x of
      File n _ -> [n]
      Dir dirName ns -> map (\z -> dirName ++ "/" ++ z) $ concatMap getFilenames ns
      _ -> []
