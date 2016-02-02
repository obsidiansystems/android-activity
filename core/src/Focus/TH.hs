{-# LANGUAGE TemplateHaskell #-}
module Focus.TH where

import Language.Haskell.TH
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

embedFile :: FilePath -> Q Exp
embedFile p = do
  l <- location
  FE.embedFile $ takeDirectory (loc_filename l) </> p

embedFileString :: FilePath -> Q Exp
embedFileString fp = appE [| T.unpack . decodeUtf8 |] (embedFile fp)

