module Networking where

import Control.Concurrent
import Control.Monad.Reader
import Data.Monoid
import System.Environment
import Text.Read

import qualified Data.ByteString.Char8 as CBS
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

newtype Communicate a = Communicate { unComm :: ReaderT Socket IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Socket)

runComm :: Communicate a -> Socket -> IO a
runComm = runReaderT . unComm

main :: IO ()
main = do getArgs >>= \case
            []     -> run echo 8080
            [port] -> case readMaybe port of
                        Just p -> run echo p
                        Nothing -> usage
            _      -> usage

usage :: IO ()
usage = do name <- getProgName
           putStrLn $ "Usage: " <> name <> " [port]"

run :: Communicate a -> Int -> IO a
run f port = withSocketsDo $ do
  addr <- resolve $ show port
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  let fd = fdSocket sock
  setCloseOnExecIfNeeded fd
  listen sock 5

  forever $ do
    (conn,peer) <- accept sock
    forkFinally (runComm f conn) (const $ close conn)

resolve port = do
  let hints = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
  return addr

write :: String -> Communicate ()
write str = do
  sock <- ask
  void $ liftIO $ send sock (CBS.pack str)

receive :: Communicate String
receive = do
  sock <- ask
  liftIO $ (init . init . CBS.unpack) <$> recv sock 1024

echo :: Communicate ()
echo = write "connection established" >> forever (receive >>= write)
