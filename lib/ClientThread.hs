module ClientThread where

import GlobalThread
import Networking

import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer hiding (listen)
import Network.Socket hiding (send, sendTo, recv, recvFrom)

import Event

data ClientEvent = ClientLoggedIn | SentMessage String | Disconnected
data ClientState = LoggedIn | NotLoggedIn | EnteredName

appendEvent :: MonadIO m => [EventTy evM] -> TVar (EventBuffer evM) -> m ()
appendEvent events buffer = do
  liftIO (readTVarIO buffer) >>= \case
    EventBuffer es -> do
      liftIO $ atomically $ writeTVar buffer (EventBuffer (es ++ events))


output :: (Monoid w, Monoid w', MonadWriter ([String], w, w') m) => String -> m ()
output s = tell ([s], mempty, mempty)

emitGlobalEvent e = tell (mempty, [EventTy e], mempty)
emitLocalEvent e = tell (mempty, mempty, [EventTy e])

-- | Client thread:
-- - output strings to send to the client
-- - output events to the global thread
-- - store local state
newtype ClientThread s a
  = ClientThread (WriterT ([String], [EventTy GlobalThread], [EventTy (ClientThread s)]) (ReaderT GlobalState (State s)) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadWriter ([String], [EventTy GlobalThread], [EventTy (ClientThread s)])
    , MonadReader GlobalState)

instance Event ClientEvent (ClientThread ()) where
  handle ClientLoggedIn = do
    output "Logged in"
    emitLocalEvent (SentMessage "AUTOMATED LOGON MESSAGE")
  handle (SentMessage s) = do --TODO:  parse message here
    globalState <- ask
    emitGlobalEvent (ConsoleLog ("Client message: " <> s))
    emitGlobalEvent (SetGlobalState (globalState + 1))
    output $ "Thanks for the message. Global state is now " <> show (globalState + 1)
  handle Disconnected = emitGlobalEvent (ConsoleLog "client disconnected")

-- | Run a client thread, sending its output to the appropriate socket
-- and dispatching any global events sent
runClientThread
  :: (MonadReader (GlobalState, Socket) m, MonadIO m, MonadState s m) =>
  TVar (EventBuffer GlobalThread)
  -> ClientThread s a -> m (a, [EventTy (ClientThread s)])
runClientThread evBuffer (ClientThread ma) = do
  (globalState, sock) <- ask
  localState <- get
  let ((x, (reply, globalEvents, localEvents)), s) = runState (runReaderT (runWriterT ma) globalState) localState
  mapM (write sock) reply
  appendEvent globalEvents evBuffer
  return (x, localEvents)
