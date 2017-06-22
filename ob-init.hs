#!/usr/bin/env runhaskell
--TODO: Don't depend on the user's environment - everything should be based on nixpkgs via reflex-platform
{-
#Obsidian Systems - Reflex Focus
#Author: Ishaq Sloan
#Desc: This script prompts user for the project name, and
# initializes the Reflex-Focus submodules along with appropriate
# directory layout fit for best developer environment results.
# This script is to be used within an existing
# github repository and assumes that you have already
# configured/added your SSH key with both github and gitlab.
-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

import System.Process
import System.Directory
import qualified Data.Text as Text
import Data.Text (Text)
import Data.ByteString as BS
import Data.ByteString (ByteString)
import Control.Monad

main :: IO ()
main = do
  --TODO consider passing an arg instead of using a prompt
  Prelude.putStr "Project name: "
  projectName <- BS.getLine
  doesDirectoryExist "focus" >>= \case
    True -> Prelude.putStrLn "Skipping focus (already exists)"
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
  baseDir <- getCurrentDirectory
  let defNix    = "default.nix"
  let main      = "Main.hs"
  let frontend  = "frontend/src" :: FilePath
  let backend   = "backend/src" :: FilePath
  doesFileExist defNix >>= \case
    True -> Prelude.putStrLn "Skipping default.nix (already exists)"
    False -> BS.writeFile defNix $ nixExpr projectName
  --TODO: C'mon... be more clever with this. Find a good function.
  setCurrentDirectory frontend
  doesFileExist main >>= \case
    True -> Prelude.putStrLn "Skipping frontend/src/Main.hs (already exist)"
    False -> Prelude.writeFile main frontSrc
  setCurrentDirectory baseDir
  setCurrentDirectory backend
  doesFileExist main >>= \case
    True -> Prelude.putStrLn "Skipping frontend/src/Main.hs (already exist)"
    False -> Prelude.writeFile main backSrc


--TODO: Consider using fileEmbed to generate default.nix in initializing folder
nixExpr :: ByteString -- ^ The name of the project; this must be a valid Cabal package name
        -> ByteString
nixExpr projectName = BS.concat
  [ "{}: (import ./focus {}).mkDerivation { name = \""
  , projectName
  , "\"; version = \"0.1\"; commonDepends = p: with p; [ data-default file-embed ]; frontendDepends = p: with p; [ data-default file-embed focus-http-th focus-js ghcjs-dom reflex reflex-dom these ]; backendDepends = p: with p; [ data-default resource-pool snap snap-core snap-loader-static snap-server ];}"
  ]

frontSrc :: String
frontSrc = "{-# LANGUAGE OverloadedStrings #-}\n\nimport Reflex.Dom\n\nmain = mainWidget $ text \"Hello, new project!\"" 

backSrc :: String
backSrc = "{-# LANGUAGE OverloadedStrings #-}\n\nimport Data.Default\nimport Focus.Backend\nimport Focus.Backend.Snap\nimport Snap\n\nmain :: IO ()\nmain = withFocus . quickHttpServe $ rootHandler\n\nrootHandler :: Snap ()\nrootHandler =\n  route [ (\"\", serveApp \"\" $ def)\n  ]"
 

mkdirs :: [FilePath] -> IO ()
mkdirs = mapM_ $ createDirectoryIfMissing True
