module Mud where

import Networking
import World
import Player

import Data.Monoid

greeting :: Communicate ()
greeting = do
  write "Welcome to the MUD!\n"
  write "What is your name?\n"
  name <- receive
  write $ "Welcome, " <> name <> "!\n"
  let player = Player Spawn name
  write (show player)

runMud :: Int -> IO ()
runMud = run greeting
