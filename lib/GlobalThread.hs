module GlobalThread where

import ConcurrentStateIO
import Event

import Control.Monad.State

-- | Placeholder global state
type GlobalState = Int


data GlobalEvent = ConsoleLog String | SetGlobalState GlobalState

-- | Global thread:
type GlobalThread = ConcurrentStateIO GlobalState

instance Event GlobalEvent GlobalThread where
  handle (ConsoleLog s) = liftIO $ putStrLn s
  handle (SetGlobalState g) = do
    liftIO $ putStrLn "Updating state"
    put g
