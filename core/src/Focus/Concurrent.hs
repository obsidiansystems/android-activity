{-# LANGUAGE ScopedTypeVariables #-}
module Focus.Concurrent where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Monoid
import Data.Either

supervise :: Show a => IO a -> IO ()
supervise a = forever $ withAsync a $ \child -> do
  result <- waitCatch child
  printResult :: Either SomeException () <- try $ putStrLn $ "supervise: child terminated with " <> show result <> "; restarting"
  threadDelay 1000000
  when (isLeft printResult) $ putStrLn "supervise: note: an exception was encountered when printing the previous result"

withLinkedThread :: IO () -> IO a -> IO a
withLinkedThread h a = withAsync (supervise h) $ \hThread -> do
  link hThread
  a

withLinkedThreads :: [IO ()] -> IO a -> IO a
withLinkedThreads [] a = a
withLinkedThreads (h:t) a = withLinkedThread h $ withLinkedThreads t a
