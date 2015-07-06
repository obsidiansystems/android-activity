{-# LANGUAGE TypeFamilies, FlexibleContexts, PolyKinds #-}
module Control.Monad.TypedUnique ( MonadTypedUnique (..)
                                 ) where

import Data.GADT.Compare
import Data.IORef

import Control.Monad.State
import Control.Monad.Reader

import Unsafe.Coerce
import System.IO.Unsafe

class (Monad m, GCompare (TypedUnique m)) => MonadTypedUnique m where
  type TypedUnique m :: k -> *
  getTypedUnique :: m (TypedUnique m a)

nextTypedUniqueIO :: IORef Int
{-# NOINLINE nextTypedUniqueIO #-}
nextTypedUniqueIO = unsafePerformIO $ newIORef 1

newtype TypedUnique_IO a = TypedUnique_IO Int deriving (Show)

instance MonadTypedUnique IO where
  type TypedUnique IO = TypedUnique_IO
  {-# INLINE getTypedUnique #-}
  getTypedUnique = do
    n <- atomicModifyIORef' nextTypedUniqueIO $ \n -> (succ n, n)
    return $ TypedUnique_IO n

instance GCompare TypedUnique_IO where
  {-# INLINE gcompare #-}
  TypedUnique_IO a `gcompare` TypedUnique_IO b = case a `compare` b of
    LT -> GLT
    EQ -> unsafeCoerce GEQ
    GT -> GGT

instance GEq TypedUnique_IO where
  {-# INLINE geq #-}
  TypedUnique_IO a `geq` TypedUnique_IO b = if a == b then Just $ unsafeCoerce Refl else Nothing

instance MonadTypedUnique m => MonadTypedUnique (StateT s m) where
  type TypedUnique (StateT s m) = TypedUnique m
  {-# INLINE getTypedUnique #-}
  getTypedUnique = lift getTypedUnique

instance MonadTypedUnique m => MonadTypedUnique (ReaderT r m) where
  type TypedUnique (ReaderT r m) = TypedUnique m
  {-# INLINE getTypedUnique #-}
  getTypedUnique = lift getTypedUnique
