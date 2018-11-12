module Mud where

import Combat
import Commands
import Networking
import Parser
import Player
import World

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Data.Monoid
import Text.Parsec (parse, ParseError)

data ClientState = LoggedIn Player | NotLoggedIn | EnteredName

newtype Mud a = Mud (ExceptT MudError (Communicate ClientState World) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader String , MonadWriter String, MonadState (ClientState, World)
    , MonadError MudError
    )

instance Alternative Mud where
  empty = Mud (lift empty)
  (Mud a) <|> (Mud b) = Mud $ let a' = runExceptT a
                                  b' = runExceptT b
                              in ExceptT (a' <|> b')
instance MonadPlus Mud where
  mzero = Mud (lift mzero)

runMud :: Mud a -> Communicate ClientState World ()
runMud (Mud ma) = runExceptT ma >>= \case
  Left e -> output $ "Error: " <> show e
  Right x -> return ()

data MudError
  = CommandParseError ParseError
  | InternalError
  | AttackError CombatError
  | CantGoThatWay
  deriving Show

greeting :: Mud ()
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
      w <- getGlobal
      case w ^? targetPlayer (p ^. pName) of
        Nothing -> playerDied
        Just _ -> return ()
      (parse command "" <$> ask) >>= \case
        Left e -> throwError (CommandParseError e)
        Right c -> case c of
          Who -> who
          Look -> look p
          Go dir -> do
            output $ "Going " <> show dir
            modifyGlobalM (movePlayer p dir) CantGoThatWay
            look p
          Help -> help
          Attack target -> attack p target

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

modifyGlobalM :: (MonadError e m, MonadState (l, g) m) => (g -> Maybe g) -> e -> m ()
modifyGlobalM f e = do
  x <- getGlobal
  case f x of
    Nothing -> throwError e
    Just x' -> setGlobal x'

server :: Int -> IO ()
server = run (runMud greeting) NotLoggedIn emptyWorld
