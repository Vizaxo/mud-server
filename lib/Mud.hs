module Mud where

import Networking
import World
import Player

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Monoid

data ClientState = LoggedIn Player | NotLoggedIn | EnteredName

greeting :: Communicate ClientState World ()
greeting = do
  getLocal >>= \case
    NotLoggedIn -> do
      ask --TODO: don't make user enter first message
      tell "Welcome to the MUD!\nWhat is your name?"
      setLocal EnteredName
    EnteredName -> do
      name <- ask
      setLocal (LoggedIn (Player Spawn name))
      tell $ "Hello, " <> name
    LoggedIn (Player location name) -> do
      tell $ name <> ", you are in " <> show location

runMud :: Int -> IO ()
runMud = run NotLoggedIn World greeting
