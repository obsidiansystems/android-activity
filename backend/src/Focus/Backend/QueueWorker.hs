module Focus.Backend.QueueWorker where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Pool
import Database.Groundhog.Postgresql

import Focus.Backend.DB
import Focus.Concurrent

worker :: (MonadIO m, RunDb f)
       => Int -- ^ Thread delay
       -> f (Pool Postgresql)
       -> FocusPersist ()
       -> m (IO ()) -- ^ Terminate action
worker delay db f = return . killThread <=< liftIO . forkIO . supervise .  void . forever $
  runDb db f >> threadDelay delay
