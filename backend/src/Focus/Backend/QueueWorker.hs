module Focus.Backend.QueueWorker where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Focus.Concurrent

worker :: MonadIO m
       => Int -- ^ Thread delay
       -> IO ()
       -> m (IO ()) -- ^ Terminate action
worker delay f = return . killThread <=< liftIO . forkIO . supervise .  void . forever $
  f >> threadDelay delay
