module MyFRP where

import Control.Applicative
import Control.Monad
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM

type DiscreteTime = Int
type ContinousTime = Double

-- Discrete time steps. Can be multiple events at the given time.
type DiscreteEvent a = [(DiscreteTime, a)]

class Runnable e m where
  run :: e -> m ()

type ContinuousEvent a = [(ContinousTime, a)]


type Buffer a = TVar [a]

emptyBuffer :: STM (Buffer a)
emptyBuffer = newTVar []

bufferFromList :: [a] -> STM (Buffer a)
bufferFromList = newTVar

popBuffer :: Buffer a -> STM a
popBuffer buf = forever $ do
  readTVar buf >>= \case
    [] -> retry
    (x:xs) -> do
      writeTVar buf xs
      return x

readBuf :: Buffer a -> IO [a]
readBuf buf = do
  readTVarIO buf >>= \case
    [] -> putStrLn "failed" >> readBuf buf
    (x:_) -> (x:) <$> (atomically (bufTail buf) >> readBuf buf)

bufTail :: Buffer a -> STM ()
bufTail buf = do
  readTVar buf >>= \case
    [] -> retry
    (_:xs) -> writeTVar buf xs

readBufferLazily :: Buffer a -> STM [a]
readBufferLazily = some . popBuffer

appendBuffer :: Buffer a -> a -> STM ()
appendBuffer buf x = readTVar buf >>= writeTVar buf . (x:)



discretise :: ContinuousEvent a -> DiscreteEvent a
discretise = undefined

forkEventful :: forall a b. (DiscreteEvent a -> DiscreteEvent b) -> DiscreteEvent a -> IO (DiscreteEvent b)
forkEventful thread evs = do
  buf <- atomically $ newTVar ([] :: DiscreteEvent b)
  forkIO $ atomically $ writeTVar buf (thread evs)
  threadDelay 1000000
  putStrLn "a"
  ret <- readBuf buf
  putStrLn "b"
  return ret

thread :: DiscreteEvent Int -> DiscreteEvent Bool
thread = map (over _2 isEven)
  where isEven x = x `mod` 2 == 0

testFork :: IO ()
testFork = do
  bools <- forkEventful thread ([1..] `zip` [1..])
  mapM_ (putStrLn . show) (take 10 bools)
