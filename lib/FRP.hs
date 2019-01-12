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

networkEvents :: MonadIO m => Int -> m (AddHandler ClientEvent)
networkEvents port = liftIO $ do
  sock <- setupSocket port
  (addHandler, fire) <- newAddHandler
  forkIO $ void $ flip runStateT 0 $ forever $ do
    clientId <- getClientId
    (conn, _) <- liftIO $ accept sock
    liftIO $ fire (clientId, Connected conn)
    liftIO $ flip forkFinally (const $ fire (clientId, Disconnected) *> close conn) $ forever $ do
      msg <- liftIO $ receive conn
      fire (clientId, Sent msg)
  return addHandler

mkNetwork :: Int -> MomentIO ()
mkNetwork port = do
  eClient <- networkEvents port >>= fromAddHandler
  clientPorts <- accumStateB emptyClientPorts (updateClients <$> eClient)
  let outputEvents = (zipTuple . over _2 processEvent <$>) $ eClient
  reactimate $ sequence_ <$> applyAtTime (liftA2 sendToClient) outputEvents (pure <$> clientPorts)

applyAtTime :: (a -> b -> c) -> Event a -> Behavior b -> Event c
applyAtTime f xs ys = (flip f <$> ys) <@> xs

accumStateB :: MonadMoment m => s -> Event (State s a) -> m (Behavior s)
accumStateB s ms = accumB s (execState <$> ms)

zipTuple :: (a, [b]) -> [(a, b)]
zipTuple (x, ys) = (x,) <$> ys

runServer :: Int -> IO ()
runServer port = do
  compile (mkNetwork port) >>= actuate
  readLn
