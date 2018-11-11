module Mud where

import Networking
import World
import Player

import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

greeting :: Socket -> IO ()
greeting s = do
  send s "Welcome to the MUD!\n"
  send s "What is your name?\n"
  name <- recv s 1024
  send s $ "Welcome, " <> BS.init (BS.init name) <> "!\n"

runMud :: Int -> IO ()
runMud = run greeting
