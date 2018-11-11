module Networking where

import Control.Concurrent
import Control.Monad
import Control.Applicative
import Data.Monoid ((<>))
import System.Environment
import Text.Read

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import qualified Network.Socket.ByteString.Lazy as LN (sendAll)

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

run :: (Socket -> IO ()) -> Int -> IO a
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
    forkFinally (f conn) (const $ close conn)

resolve port = do
  let hints = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
  return addr

echo :: Socket -> IO ()
echo s = send s "connection established" >> forever (recv s 1024 >>= send s)
