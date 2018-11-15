module Run where

import Event
import Networking
import ConcurrentStateIO

import ClientThread
import GlobalThread

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Network.Socket hiding (send, sendTo, recv, recvFrom)

-- | Run a thread which handles events, updating a buffer of global events.
runEventThread
  :: forall m s.
  Socket
  -> TVar (EventBuffer (ClientThread s))
  -> TVar (EventBuffer GlobalThread)
  -> TVar GlobalState
  -> s
  -> IO ()
runEventThread sock localEvents globalEvents globalState initialState = do
  forever $ flip runStateT initialState $ liftIO (readTVarIO localEvents) >>= \case
    EventBuffer [] -> return ()
    EventBuffer (e:es) -> do
      -- Handle all of the events until there are none left
      let threadComputation = runClientThread globalEvents $ handle e
      gSt <- liftIO $ readTVarIO globalState
      lSt <- get
      ((_, es'), lSt') <- liftIO $ runStateT (runReaderT threadComputation (gSt, sock)) lSt
      put lSt'
      liftIO $ atomically $ writeTVar localEvents (EventBuffer (es ++ es'))

--TODO: make it a TMVar to avoid constant looping on empty buffer?
-- | Perform the given function on every element that is added to a
-- buffer. Will loop forever waiting for new events.
forAllInBuffer :: MonadIO m => TVar (EventBuffer m) -> (EventTy m -> m ()) -> m ()
forAllInBuffer buf run = forever $ liftIO (readTVarIO buf) >>= \case
  EventBuffer [] -> return ()
  EventBuffer (e:es) -> do
    run e
    liftIO $ atomically $ writeTVar buf (EventBuffer es)

-- | Continually run a computation while it returns True. Stop if
-- False is returned.
whileTrue :: Monad m => m Bool -> m ()
whileTrue mb = mb >>= \case
  True -> whileTrue mb
  False -> return ()

-- | Communicate with a client, sending the appropriate events when it
-- connects, sends a message, and disconnects.
clientCommunicationThread :: Socket -> TVar (EventBuffer (ClientThread ())) -> IO ()
clientCommunicationThread sock evBuffer = do
  appendEvent [EventTy ClientLoggedIn] evBuffer
  whileTrue $ runMaybeT (receive sock) >>= \case
    Nothing -> False <$ appendEvent [EventTy Disconnected] evBuffer
    Just msg -> True <$ appendEvent [EventTy $ SentMessage msg] evBuffer

-- | Run the server on the given port.
run :: Int -> IO ()
run port = do
  sock <- setupSocket port

  globalState <- newTVarIO 10
  globalEvents <- newTVarIO (EventBuffer [] :: EventBuffer GlobalThread)

  -- Global thread
  forkIO $ void $ runConcurrentStateIO (forAllInBuffer globalEvents handle) globalState

  -- Client threads
  forever $ do
    (conn,peer) <- accept sock
    threadEvents <- newTVarIO (EventBuffer [])

    -- Event handler thread
    forkIO (runEventThread conn threadEvents globalEvents globalState ())

    -- Message receiver thread
    forkFinally (clientCommunicationThread conn threadEvents) (const $ close conn)
