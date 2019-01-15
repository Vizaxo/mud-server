module FRP where

import Control.Concurrent
import Control.Lens
import Control.Monad.State
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import Network.Socket hiding (send, sendTo, recv, recvFrom, Connected)

import Client
import Networking
import Event
import Mud
import GameState
import LoadSave
import ConsoleControl

-- | Set up network inputs on the given port to trigger FRP events
networkInputEvents :: MonadIO m => Int -> m (AddHandler ClientEvent)
networkInputEvents port = liftIO $ do
  sock <- setupSocket port
  (addHandler, fire) <- newAddHandler
  forkIO $ void $ flip runStateT 0 $ forever $ do
    clientId <- freshClientId
    (conn, _) <- liftIO $ accept sock
    liftIO $ fire (clientId, Connected conn)
    liftIO $ flip forkFinally (const $ fire (clientId, Disconnected) *> close conn) $ forever $ do
      msg <- liftIO $ receive conn
      fire (clientId, Sent msg)
  return addHandler

-- | Connect the FRP network
mkNetwork :: Int -> GameState -> AddHandler ConsoleEvent -> MomentIO ()
mkNetwork port initialGameState consoleEventHandler = do
  -- Receive and process network events
  inputEvents <- networkInputEvents port >>= fromAddHandler
  clientPorts <- accumStateB emptyClientPorts (updateClients <$> inputEvents)
  (outputEvents, gameState) <- mapAccum initialGameState (processEvent <$> inputEvents)

  -- TODO: why don't the first messages send? Probably something to do with the accumulation of the state not updating in time

  -- Receive and process console events
  consoleEvents <- (processConsoleEvent <$>) <$> fromAddHandler consoleEventHandler
  reactimate $ sequence_ <$> applyAtTime (liftA2 handleConsoleProcess) (pure <$> gameState) consoleEvents

  -- Trigger the output events to send to the client
  reactimate $ sequence_ <$> applyAtTime (liftA2 sendToClient) (pure <$> clientPorts) outputEvents

-- | Apply the function f to its arguments, taken from the given event and behavior
applyAtTime :: (a -> b -> c) -> Behavior a -> Event b -> Event c
applyAtTime f xs ys = (f <$> xs) <@> ys

-- | Turn an event of stateful computations into a behaviour accumulating the state
accumStateB :: MonadMoment m => s -> Event (State s a) -> m (Behavior s)
accumStateB s ms = accumB s (execState <$> ms)

-- | Distribute the first element of a tuple over the list in the second element
zipTuple :: (a, [b]) -> [(a, b)]
zipTuple (x, ys) = (x,) <$> ys

-- | Run the game server on the given port
runServer :: Int -> IO ()
runServer port = do
  (consoleEventHandler, fireConsoleEvent) <- newAddHandler
  loadOrCreateSave >>= \case
    Left e -> putStrLn "Error: file 'world' is corrupt"
    Right initialGameState -> do
      compile (mkNetwork port initialGameState consoleEventHandler) >>= actuate
      forever $ fireConsoleEvent . ConsoleMessage =<< getLine
