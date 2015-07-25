{-# LANGUAGE ScopedTypeVariables #-}
module Focus.Concurrent where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Monoid
import Data.Either

supervise :: Show a => IO a -> IO ()
supervise a = forever $ withAsync a $ \child -> do
  result <- waitCatch child
  printResult :: Either SomeException () <- try $ putStrLn $ "supervise: child terminated with " <> show result <> "; restarting"
  when (isLeft printResult) $ putStrLn "supervise: note: an exception was encountered when printing the previous result"
