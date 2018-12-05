module FRP where

import System.IO
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Reactive.Banana.Frameworks
import Network.Socket hiding (send, sendTo, recv, recvFrom, Connected)
import GHC.Exception

import Text.Parsec
import Parser
import Networking

data Event = Connected | Sent String | Disconnected
  deriving Show

type ClientId = Int
type ClientEvent = (ClientId, Event)

networkEvents :: MonadIO m => Int -> m (AddHandler ClientEvent)
networkEvents port = liftIO $ do
  sock <- setupSocket port
  (addHandler, fire) <- newAddHandler
  forkIO $ void $ flip runStateT 0 $ forever $ do
    clientId <- getClientId
    (conn, _) <- liftIO $ accept sock
    liftIO $ fire (clientId, Connected)
    liftIO $ flip forkFinally (const $ fire (clientId, Disconnected) *> close conn) $ forever $ do
      msg <- liftIO $ receive conn
      fire (clientId, Sent msg)
  return addHandler

getClientId :: MonadState ClientId m => m ClientId
getClientId = get <* modify (+1)

mkNetwork :: Int -> MomentIO ()
mkNetwork port = do
  eClient <- networkEvents port >>= fromAddHandler
  reactimate $ parse command "" <$> eClient

runServer :: Int -> IO ()
runServer port = do
  compile (mkNetwork port) >>= actuate
  readLn

echo = do
  (keyEventHandler, fire) <- newAddHandler

  -- Network Specification (echo keyboard input)
  let networkDescription =
        fromAddHandler keyEventHandler >>= -- Create event stream from handler
        reactimate . fmap print -- Map print over event stream

  compile networkDescription >>= actuate

  -- Event Loop
  hSetBuffering stdin NoBuffering
  forever $ do
      ready <- hReady stdin
      if ready
          then getChar >>= fire -- Create keyboad event
          else return ()
