{-# LANGUAGE TemplateHaskell #-}
module Focus.TH where

import qualified Data.ByteString as B
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.FileEmbed as FE
import System.FilePath
import qualified Data.Text as T
import Data.Text.Encoding

conName :: Con -> Name
conName x = case x of
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ c -> conName c

qReadFile :: FilePath -> Q B.ByteString
qReadFile path = do
  l <- location
  let file = takeDirectory (loc_filename l) </> path
  qAddDependentFile file
  runIO $ B.readFile file

embedFile :: FilePath -> Q Exp
embedFile p = do
  l <- location
  FE.embedFile $ takeDirectory (loc_filename l) </> p

embedFileString :: FilePath -> Q Exp
embedFileString fp = appE [| T.unpack . decodeUtf8 |] (embedFile fp)

