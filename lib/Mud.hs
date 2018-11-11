module Mud where

import Commands
import Networking
import Parser
import Player
import World

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Monoid
import Text.Parsec

data ClientState = LoggedIn Player | NotLoggedIn | EnteredName

greeting :: Communicate ClientState World ()
greeting = do
  getLocal >>= \case
    NotLoggedIn -> do
      --TODO: don't make user enter first message
      tell "Welcome to the MUD!\nWhat is your name?"
      setLocal EnteredName
    EnteredName -> do
      name <- ask
      let player = Player name
      setLocal (LoggedIn player)
      modifyGlobal (addPlayer player Spawn)
      tell $ "Hello, " <> name
    LoggedIn p -> do
      (parse command "" <$> ask) >>= \case
        Left _ -> tell "Sorry, I did not understand that."
        Right c -> case c of
          Who -> who
          Look -> look p
          Go dir -> tell $ "Going " <> show dir

who :: Communicate ClientState World ()
who = do
  World players <- getGlobal
  tell ("The following players are logged in: " <> show ((fst) <$> players) <> "\n")

look :: Player -> Communicate ClientState World ()
look p@(Player name) = do
  World players <- getGlobal
  case lookup p players of
    Nothing -> tell "Not found in list of logged-in players."
    Just location -> tell $ name <> ", you are in " <> show location

runMud :: Int -> IO ()
runMud = run greeting NotLoggedIn (World [])
