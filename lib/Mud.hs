module Mud where

import Control.Monad.State
import Data.Text
import Data.Monoid
import Text.Parsec hiding (State)
import Network.Socket (Socket)

import Parser
import Commands
import Combat
import Event
import World

data MudError
  = CommandParseError ParseError
  | InternalError
  | AttackError CombatError
  | CantGoThatWay
  deriving Show

data OutputEvent
  = Message Text
  | OutputError MudError
  deriving Show

processEvent :: (ClientId, InputEvent) -> State World [(ClientId, OutputEvent)]
processEvent (id, ev) = pure $ (id,) <$> case ev of
  Connected sock -> do
    [Message "Welcome to the MUD!", Message "What is your name?"]
    -- setLocal EnteredName
  {-
  EnteredName -> do
    name <- ask
    let player = mkPlayer name
    setLocal (LoggedIn player)
    modifyGlobal (addPlayer player spawn)
    output $ "Hello, " <> name
-}
  Sent msg ->
    case (parse command "" msg) of
      Left e -> [OutputError (CommandParseError e)]
      Right c -> case c of
        Who -> [Message "who"]
        Look -> [Message "look"]
        Go dir ->
          [Message $ "Going " <> pack (show dir)]
          --modifyGlobalM (movePlayer p dir) CantGoThatWay
          --look p
        Help -> [Message "help"]
        Attack target -> [Message $ "attack " <> pack (show target)]--attack p target

{-
playerDied :: Mud ()
playerDied = do
  output "You have died. Goodbye."
  disconnect

who :: Mud ()
who = do
  World players <- getGlobal
  output ("The following players are logged in: " <> show (fst <$> M.toList players))

look :: Player -> Mud ()
look p@(Player name stats) = do
  w <- getGlobal
  case M.lookup name (w ^. wPlayers) of
    Nothing -> throwError InternalError
    Just (_, l) -> do
      output (locName l)
      output "---"
      output (description l)
      output ("Exits: " <> showExits (exits l))
      output ("Players here: " <> show (showPlayer <$> (playersAtLocation l w)))
  where
    showExits = show . (fst <$>) . M.toList

help :: Mud ()
help = output "The following commands are available: who, look, go <direction>, help, attack <target>"

attack :: Player -> String -> Mud ()
attack p target = do
  w <- getGlobal
  getLocation p >>= \case
    Nothing -> throwError InternalError
    Just myLoc ->
      case M.lookup target (w ^. wPlayers) of
        Nothing -> throwError (AttackError TargetNotFound)
        Just (t, tLoc) -> do
          if (locName tLoc == locName myLoc)
            then do
              w <- getGlobal
              case strike p t of
                Just p' -> setGlobal (World $ M.insert target (p', tLoc) (w ^. wPlayers))
                Nothing -> do
                  setGlobal (World $ sans target (w ^. wPlayers))
            else throwError (AttackError TargetNotNear)

getLocation :: Player -> Mud (Maybe Location)
getLocation p = do
  w <- getGlobal
  return (snd <$> (M.lookup (p ^. pName) (w ^. wPlayers)))
-}
