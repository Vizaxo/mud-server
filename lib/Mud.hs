module Mud where

import Commands
import Networking
import Parser
import Player
import World

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Text.Parsec

data ClientState = LoggedIn Player | NotLoggedIn | EnteredName

type Comm = Communicate ClientState World ()

greeting :: Comm
greeting = do
  getLocal >>= \case
    NotLoggedIn -> do
      --TODO: don't make user enter first message
      output "Welcome to the MUD!"
      output "What is your name?"
      setLocal EnteredName
    EnteredName -> do
      name <- ask
      let player = Player name
      setLocal (LoggedIn player)
      modifyGlobal (addPlayer player spawn)
      output $ "Hello, " <> name
    LoggedIn p -> do
      (parse command "" <$> ask) >>= \case
        Left _ -> output "Sorry, I did not understand that."
        Right c -> case c of
          Who -> who
          Look -> look p
          Go dir -> do
            output $ "Going " <> show dir
            modifyGlobal (movePlayer p dir)
          Help -> help

who :: Comm
who = do
  World players <- getGlobal
  output ("The following players are logged in: " <> show ((fst) <$> M.toList players))

look :: Player -> Comm
look p@(Player name) = do
  World players <- getGlobal
  case M.lookup p players of
    Nothing -> output "Not found in list of logged-in players."
    Just (Location loc desc exits) -> do
      output loc
      output "---"
      output desc
      output ("Exits: " <> showExits exits)
      output ("Players here: " <> playersHere loc players)
  where
    showExits :: Map Direction Location -> String
    showExits = show . (fst <$>) . M.toList

    playersHere :: String -> Map Player Location -> String
    playersHere loc = show . (pName . fst <$>) . filter (\(p, (Location l _ _)) -> l == loc) . M.toList

help :: Comm
help = output "The following commands are available: who, look, go <direction>, help"

runMud :: Int -> IO ()
runMud = run greeting NotLoggedIn emptyWorld
