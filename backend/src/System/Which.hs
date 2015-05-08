{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module System.Which where

import Shelly (shelly)
import qualified Shelly as Sh
import qualified Data.Text as T
import Language.Haskell.TH
import Control.Monad
import Data.Monoid

-- | Determine which executable would run if the given path were executed, or return Nothing if a suitable executable cannot be found
which :: FilePath -> IO (Maybe FilePath)
which f = liftM (fmap (T.unpack . Sh.toTextIgnore)) $ shelly $ Sh.which $ Sh.fromText $ T.pack f

-- | Run `which` at compile time, and substitute the full path to the executable
--
-- This is useful in NixOS to ensure that the resulting executable contains the dependency in its closure and that it refers to the same version at run time as at compile time
staticWhich :: FilePath -> Q Exp
staticWhich f = do
  mf' <- runIO $ which f
  case mf' of
    Nothing -> do
      let err = "staticWhich: Could not find executable " <> show f
      reportError err
      [| error err |]
    Just f' -> [| f' |]
