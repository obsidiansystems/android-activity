module Focus.UniqueId where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Random.TF.Init
import System.Random.TF.Instances

base32digits :: Map Int Char
base32digits = Map.fromList $ zip [0..] "0123456789abcdefghjkmnpqrstvwxyz"

generateBase32UID :: IO Text
generateBase32UID = fmap (T.pack . map (\k -> Map.findWithDefault '0' k base32digits) . take 22 . randomRs (0,31)) $ initTFGen
