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
      let player = Player name
      setLocal (LoggedIn player)
      modifyGlobal (addPlayer player Spawn)
      tell $ "Hello, " <> name
    LoggedIn p@(Player name) -> do
      World players <- getGlobal
      tell ("The following players are logged in: " <> show ((fst) <$> players) <> "\n")
      case lookup p players of
        Nothing -> tell "Not found in list of logged-in players."
        Just location -> tell $ name <> ", you are in " <> show location

runMud :: Int -> IO ()
runMud = run greeting NotLoggedIn (World [])
