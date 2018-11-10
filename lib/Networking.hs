{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Networking where

import Control.Concurrent
import Control.Monad
import Data.Monoid ((<>))
import System.Environment
import Text.Read

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import qualified Network.Socket.ByteString.Lazy as LN (sendAll)

main :: IO ()
main = do getArgs >>= \case
            []     -> run 8080
            [port] -> case readMaybe port of
                        Just p -> run p
                        Nothing -> usage
            _      -> usage

usage :: IO ()
usage = do name <- getProgName
           putStrLn $ "Usage: " <> name <> " [port]"

run :: Int -> IO ()
run port = withSocketsDo $ do
  addr <- resolve $ show port
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  let fd = fdSocket sock
  setCloseOnExecIfNeeded fd
  listen sock 5

  forever $ do
    (conn,peer) <- accept sock
    void $ forkFinally (respond conn) (const $ close conn)

resolve port = do
  let hints = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
  return addr

respond :: Socket -> IO ()
respond s = send s "connection established" >> forever (recv s 1024 >>= send s)
