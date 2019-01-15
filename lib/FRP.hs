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
import ConsoleControl
import Utils

-- | Run the game server on the given port
runServer :: Int -> IO ()
runServer port = do
  (consoleEventHandler, fireConsoleEvent) <- newAddHandler
  compile (mkNetwork port newGameState consoleEventHandler) >>= actuate
  forever $ fireConsoleEvent . ConsoleMessage =<< getLine

-- | Connect the FRP network
mkNetwork :: Int -> GameState -> AddHandler ConsoleEvent -> MomentIO ()
mkNetwork port initialGameState consoleEventHandler = do
  -- Receive and process network events
  inputEvents <- networkInputEvents port >>= fromAddHandler
  (outputEvents, gameState) <- mapAccum initialGameState (processEvent <$> inputEvents)
  clientPorts <- accumStateE emptyClientPorts (updateClients <$> inputEvents)

  -- Receive and process console events
  consoleEvents <- (processConsoleEvent <$>) <$> fromAddHandler consoleEventHandler
  reactimate $ sequence_ <$> applyAtTime (liftA2 handleConsoleProcess) (pure <$> gameState) consoleEvents

  -- Trigger the output events to send to the client
  -- Ideally, clientPorts would be a Behavior, which would negate the
  -- use for unionTuple. However, due to accumB triggering the changes
  -- after a tiny delay, the updated value would not be present and
  -- messages sent in reply to a new connection would fail to send.
  let eventsAndPorts = zipTuple <$> unionTuple clientPorts outputEvents
  reactimate $ sequence_ . (uncurry sendToClient <$>) <$> eventsAndPorts

-- | Set up network inputs on the given port to trigger FRP events
networkInputEvents :: MonadIO m => Int -> m (AddHandler ClientEvent)
networkInputEvents port = liftIO $ do
  sock <- setupSocket port
  (addHandler, fire) <- newAddHandler
  forkIO $ void $ flip runStateT 0 $ forever $ do
    clientId <- freshClientId
    (conn, _) <- liftIO $ accept sock
    liftIO $ fire (clientId, Connected conn)
    let disconnect = const $ fire (clientId, Disconnected) *> close conn
    liftIO $ flip forkFinally disconnect $ forever $ do
      msg <- liftIO $ receive conn
      fire (clientId, Sent msg)
  return addHandler

-- | Union two events into a tuple, only keeping the pairs of events
-- which trigger simultaneously
unionTuple :: Event a -> Event b -> Event (a,b)
unionTuple a b = filterJust $ rightToJust <$> joinedEvent
  where
    joinedEvent = unionWith eitherToTuple (Left . Left <$> a) (Left . Right <$> b)
    eitherToTuple (Left (Left x)) (Left (Right y)) = Right (x,y)

-- | Apply the function f to its arguments, taken from the given event
-- and behavior
applyAtTime :: (a -> b -> c) -> Behavior a -> Event b -> Event c
applyAtTime f xs ys = (f <$> xs) <@> ys

-- | Turn an event of stateful computations into a behaviour
-- accumulating the state
accumStateE :: MonadMoment m => s -> Event (State s a) -> m (Event s)
accumStateE s ms = accumE s (execState <$> ms)

-- | Distribute the first element of a tuple over the list in the
-- second element
zipTuple :: (a, [b]) -> [(a, b)]
zipTuple (x, ys) = (x,) <$> ys
