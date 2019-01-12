module Mud where

import Control.Arrow
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Lens
import Data.Text
import Data.Map as M
import Data.Monoid
import Text.Parsec hiding (State)
import Network.Socket (Socket)

import Parser
import Commands
import Combat
import Event
import World
import GameState

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

type Messages = [(ClientId, OutputEvent)]

processEvent :: (ClientId, InputEvent) -> GameState -> ([(ClientId, OutputEvent)], GameState)
processEvent (cId, ev) = runState $ flip runReaderT cId $ execWriterT $ processEvent' ev

processEvent' :: (MonadReader ClientId m, MonadState GameState m, MonadWriter Messages m) => InputEvent -> m ()
processEvent' (Connected sock) = do
      reply "Welcome to the MUD!"
      reply "What is your name?"
      cId <- ask
      modify (over gsPlayers (M.insert cId EnteringName))
processEvent' (Sent msg) = do
  cId <- ask
  worldState <- get
  case M.lookup cId (view gsPlayers worldState) of
    Nothing -> clientError InternalError
    Just s -> case s of
      EnteringName -> do
        cId <- ask
        pId <- freshPId
        modify (over gsPlayers (M.insert cId (InGame pId)))
      InGame pId -> case (parse command "" msg) of
        Left e -> clientError (CommandParseError e)
        Right c -> case c of
          Who -> reply "who"
          Look -> reply "look"
          Go dir ->
            reply $ "Going " <> pack (show dir)
            --modifyGlobalM (movePlayer p dir) CantGoThatWay
            --look p
          Help -> reply "help"
          Attack target -> reply $ "attack " <> pack (show target)--attack p target
    {-
    EnteredName -> do
      name <- ask
      let player = mkPlayer name
      setLocal (LoggedIn player)
      modifyGlobal (addPlayer player spawn)
      output $ "Hello, " <> name
  -}

reply :: (MonadReader ClientId m, MonadWriter Messages m) => Text -> m ()
reply msg = do
  cId <- ask
  tell [(cId, Message msg)]

clientError :: (MonadReader ClientId m, MonadWriter Messages m) => MudError -> m ()
clientError err = do
  cId <- ask
  tell [(cId, OutputError err)]

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
