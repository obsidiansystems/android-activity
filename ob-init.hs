#!/usr/bin/env runhaskell
-- |TODO: Don't depend on the user's environment - everything should be based on nixpkgs via reflex-platform
{-|
 Description: This script prompts user for the project name, and
 initializes the Reflex-Focus submodules along with appropriate
 directory layout fit for best developer environment results.
 This script is to be used within an existing
 github repository and assumes that you have already
 configured/added your SSH key with both github and gitlab.
-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

import System.Process (callProcess)
import System.Directory ( doesDirectoryExist
                        , createDirectoryIfMissing
                        , doesFileExist)
import Data.Text (Text)
import Data.FileEmbed (embedStringFile)

main :: IO ()
main = do
  --TODO consider passing an arg instead of using a prompt
  putStr "Project name: "
  projectName <- getLine
  doesDirectoryExist "focus" >>= \case
    True -> putStrLn "Skipping focus (already exists)"
    False -> do
      callProcess "git"
        [ "submodule"
        , "add"
        , "-b"
        , "develop"
        , "git@gitlab.com:obsidian.systems/focus"
        ]
      callProcess "git"
        [ "submodule"
        , "update"
        , "--init"
        , "--recursive"
        ]
  mkdirs ["common/src", "frontend/src", "backend/src", "static", "config"]
  let defNix    = "default.nix"
  let frontend  = "frontend/src/Main.hs" :: FilePath
  let backend   = "backend/src/Main.hs" :: FilePath
  -- Warning: Data.FileEmbed (embedFile functions) may not work with all environments.
  createFileIfMissing defNix $(embedStringFile "ob-init-default.nix")
  createFileIfMissing frontend frontSrc
  createFileIfMissing backend backSrc

{-
--TODO: Consider using fileEmbed to generate default.nix in initializing folder
nixExpr :: String -- ^ The name of the project; this must be a valid Cabal package name
        -> String
nixExpr projectName = USE UNLINES TO MERGE PROJECT NAME WITH default.nix code
-}
frontSrc :: String
frontSrc = "{-# LANGUAGE OverloadedStrings #-}\n\nimport Reflex.Dom\n\nmain = mainWidget $ text \"Hello, new project!\"" 

backSrc :: String
backSrc = "{-# LANGUAGE OverloadedStrings #-}\n\nimport Data.Default\nimport Focus.Backend\nimport Focus.Backend.Snap\nimport Snap\n\nmain :: IO ()\nmain = withFocus . quickHttpServe $ rootHandler\n\nrootHandler :: Snap ()\nrootHandler =\n  route [ (\"\", serveApp \"\" $ def)\n  ]"
 

mkdirs :: [FilePath] -> IO ()
mkdirs = mapM_ $ createDirectoryIfMissing True

createFileIfMissing :: FilePath -> String -> IO ()
createFileIfMissing aFile content = 
  doesFileExist aFile >>= \case
    True -> putStrLn "Skipping frontend/src/Main.hs (already exist)"
    False -> writeFile aFile content
