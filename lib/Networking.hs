module Networking where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Writer hiding (listen)
import Control.Monad.State
import Data.Monoid
import System.Environment
import Text.Read hiding (get)

import qualified Data.ByteString.Char8 as CBS
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

-- | Communication monad.
-- Reader String: user input
-- Writer String: output to user
-- State l: connection-local state
-- State g: global state
newtype Communicate l g a = Communicate { unComm :: ReaderT String (WriterT String (State (l, g))) a }
  deriving (Functor, Applicative, Monad, MonadReader String, MonadWriter String, MonadState (l, g))

main :: IO ()
main = do getArgs >>= \case
            []     -> run 0 0 echo 8080
            [port] -> case readMaybe port of
                        Just p -> run 0 0 echo p
                        Nothing -> usage
            _      -> usage

usage :: IO ()
usage = do name <- getProgName
           putStrLn $ "Usage: " <> name <> " [port]"

run :: forall l g a. l -> g -> Communicate l g a -> Int -> IO a
run l g f port = withSocketsDo $ do
  addr <- resolve $ show port
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  let fd = fdSocket sock
  setCloseOnExecIfNeeded fd
  listen sock 5

  global <- newTVarIO g

  forever $ do
    (conn,peer) <- accept sock
    forkFinally (run conn l global) (const $ close conn)
  where
    run :: Socket -> l -> TVar g -> IO ()
    run conn l global = do
      msg <- receive conn
      when (msg == ":quit") mzero
      (l', output) <- atomically $ do
        g <- readTVar global
        let ((x, output), (l', g')) = runState (runWriterT (runReaderT (unComm f) msg)) (l, g)
        writeTVar global g'
        return (l', output)
      write (output <> "\n") conn
      run conn l' global

resolve port = do
  let hints = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
  return addr

write str sock = void $ send sock (CBS.pack str)

receive sock = do
  msg <- recv sock 1024
  when (CBS.length msg == 0) mzero
  return (init . init . CBS.unpack $ msg)

-- | Echoes back what the user wrote. Keeps a count of the numeber of
-- messages sent per connection, and the total number of messages sent
-- to the server.
echo :: Communicate Int Int ()
echo = do
  ask >>= tell
  (l, g) <- get
  tell ("local count:" <> show l <> "\n")
  tell ("global count:" <> show g)
  modify (\(l, g) -> ((l+1), (g+1)))

setLocal :: l -> Communicate l g ()
setLocal l = modify (\(_, g) -> (l, g))

setGlobal :: g -> Communicate l g ()
setGlobal g = modify (\(l, _) -> (l, g))

getLocal :: Communicate l g l
getLocal = fst <$> get

getGlobal :: Communicate l g g
getGlobal = snd <$> get

modifyLocal :: (l -> l) -> Communicate l g ()
modifyLocal f = modify (\(l, g) -> (f l, g))

modifyGlobal :: (g -> g) -> Communicate l g ()
modifyGlobal f = modify (\(l, g) -> (l, f g))
