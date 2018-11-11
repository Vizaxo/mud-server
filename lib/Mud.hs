module Mud where

import Combat
import Commands
import Networking
import Parser
import Player
import World

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Data.Monoid
import Text.Parsec

data ClientState = LoggedIn Player | NotLoggedIn | EnteredName

type Comm a = Communicate ClientState World a

greeting :: Comm ()
greeting = do
  getLocal >>= \case
    NotLoggedIn -> do
      --TODO: don't make user enter first message
      output "Welcome to the MUD!"
      output "What is your name?"
      setLocal EnteredName
    EnteredName -> do
      name <- ask
      let player = mkPlayer name
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
          Attack target -> attack p target

who :: Comm ()
who = do
  World players <- getGlobal
  output ("The following players are logged in: " <> show ((fst) <$> M.toList players))

look :: Player -> Comm ()
look p@(Player name stats) = do
  w <- getGlobal
  case M.lookup name (w ^. wPlayers) of
    Nothing -> output "Not found in list of logged-in players."
    Just (_, l@(Location name desc exits)) -> do
      output name
      output "---"
      output desc
      output ("Exits: " <> showExits exits)
      output ("Players here: " <> show (playersAtLocation l w))
  where
    showExits = show . (fst <$>) . M.toList

help :: Comm ()
help = output "The following commands are available: who, look, go <direction>, help, attack <target>"

attack :: Player -> String -> Comm ()
attack p target = do
  w <- getGlobal
  getLocation p >>= \case
    Nothing -> output "Error"
    Just myLoc ->
      case M.lookup target (w ^. wPlayers) of
        Nothing -> output "Error"
        Just (t, tLoc) -> do
          if (locName tLoc == locName myLoc)
            then modifyGlobal (\(World ps) -> World (M.insert target (strike p t, tLoc) ps))
            else output "Target is not near you"

getLocation :: Player -> Comm (Maybe Location)
getLocation p = do
  w <- getGlobal
  return (snd <$> (M.lookup (p ^. pName) (w ^. wPlayers)))

runMud :: Int -> IO ()
runMud = run greeting NotLoggedIn emptyWorld
