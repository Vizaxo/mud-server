module ConcurrentStateIO where

import Control.Concurrent.STM
import Control.Monad.State

-- | A concurrent state monad using an STM variable to store the state.
data ConcurrentStateIO s a = ConcurrentStateIO { runConcurrentStateIO :: TVar s -> IO (s, a) }


instance Functor (ConcurrentStateIO s) where
  fmap = liftM

instance Applicative (ConcurrentStateIO s) where
  pure = return
  (<*>) = ap

instance Monad (ConcurrentStateIO s) where
  return x = ConcurrentStateIO $ ((,x) <$>) . readTVarIO

  mx >>= mf = ConcurrentStateIO (\buf -> do
    (s, x) <- runConcurrentStateIO mx buf
    atomically $ writeTVar buf s
    (s', res) <- runConcurrentStateIO (mf x) buf
    atomically $ writeTVar buf s'
    return (s', res))

instance MonadIO (ConcurrentStateIO s) where
  liftIO ma = ConcurrentStateIO (\buf -> (,) <$> readTVarIO buf <*> ma)

instance MonadState s (ConcurrentStateIO s) where
  put s = ConcurrentStateIO (\buf -> (s,) <$> (atomically $ writeTVar buf s))
  get = ConcurrentStateIO (((\s -> (s, s)) <$>) . readTVarIO)
