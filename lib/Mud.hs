module Mud where

import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Lens
import Data.Text
import Data.Map as M
import Data.Maybe
import Data.Monoid
import Text.Parsec (parse, eof)
import Network.Socket (Socket)

import Parser
import Player
import Stats
import Commands
import Combat
import Event
import World
import GameState
import Utils

type Messages = [(ClientId, OutputEvent)]

-- | A convenient type synonym for the monadic operations the Mud supports
type MonadMud m =
  ( MonadReader ClientId m
  , MonadState GameState m
  , MonadWriter Messages m
  , MonadError MudError m
  )

-- | The pure version of @processEvent'@, which is used in the FRP module.
processEvent :: (ClientId, InputEvent) -> GameState -> ([(ClientId, OutputEvent)], GameState)
processEvent (cId, ev) = runState $ flip runReaderT cId $ sendErrors $ execWriterT $ processEvent' ev

-- | Send thrown errors to the client
sendErrors :: MonadReader ClientId m => ExceptT MudError m Messages -> m Messages
sendErrors m = runExceptT m >>= \case
                 Left e -> do
                   cId <- ask
                   return [(cId, OutputError e), (cId, Message "")]
                 Right m -> return m

-- | Process a client's message. This can update the game state,
-- and/or send messages to any of the clients.
processEvent' :: MonadMud m => InputEvent -> m ()
processEvent' (Connected _) = welcomeMessage
processEvent' Disconnected = mapM_ removePlayer =<< getCurrentPid
processEvent' (Sent msg) = do
  cId <- ask
  gs <- get
  maybeThrow InternalError (M.lookup cId (gs ^. gsPlayers)) >>= \case
    EnteringName -> enteringName msg
    InGame pId -> inGame msg pId
  newline

-- | The player has sent a message, and is currently logged into the game
inGame :: MonadMud m => String -> PlayerId -> m ()
inGame msg pId = withError CommandParseError (parse command "" msg) >>= \case
  Who -> who
  Look -> look pId
  Go dir -> go pId dir
  Help -> help
  Attack target -> attack pId target
  Logout -> logout pId
  Whisper target whisperMsg -> whisper pId target whisperMsg

-- | The player has sent a message, which should contain the name they
-- want to log in with
enteringName :: MonadMud m => String -> m ()
enteringName nameAttempt = do
  cId <- ask
  name <- withError InvalidName (parse (playerName <* eof) "" nameAttempt)
  pId <- freshPId
  modify (over gsPlayers (M.insert cId (InGame pId)))
  modify (over (gsWorld . wPlayers) (M.insert pId (Player pId name defaultStats, spawn)))
  reply $ "Welcome, " <> pack name
  reply "Type help to show the available commands"

-- | Move the player in the given direction
go :: MonadMud m => Int -> Direction -> m ()
go pId dir = do
  gs <- get
  let w = gs ^. gsWorld
  (p, l) <- maybeThrow InternalError (M.lookup pId (w ^. wPlayers))
  w <- maybeThrow CantGoThatWay (movePlayer p dir w)
  modify (set gsWorld w)
  look pId

-- | Whisper to another player
whisper :: MonadMud m => PlayerId -> String -> String -> m ()
whisper pId target whisperMsg = do
  gs <- get
  let w = gs ^. gsWorld
  name <- fromMaybe "UNKNOWN" <$> getPlayerName pId
  (t, l) <- maybeThrow WhisperTargetNotFound (targetPlayer target w)
  sendToPlayer (t ^. playerId) (Message $ "Message received from " <> pack name <> ": " <> pack whisperMsg)

-- | Log the player out
logout pId = do
  sendToCurrentClient Disconnect
  removePlayer pId

-- | If the current player is logged into the game, get their player ID.
getCurrentPid :: MonadMud m => m (Maybe PlayerId)
getCurrentPid = do
  cId <- ask
  gs <- get
  return (M.lookup cId (gs ^. gsPlayers) >>= (^? _InGame))

-- | Send a blank line to the current client
newline :: MonadMud m => m ()
newline = reply ""

-- | Send a welcome message when a new client connects
welcomeMessage :: MonadMud m => m ()
welcomeMessage = do
  reply "Welcome to the MUD!"
  reply "What is your name?"
  cId <- ask
  modify (over gsPlayers (M.insert cId EnteringName))

-- | List the players who are currently online.
who :: MonadMud m => m ()
who = do
  players <- (^. gsWorld . wPlayers) <$> get
  reply ("The following players are logged in: " <> (pack $ show $ view (_1 . pName) <$> M.elems players))

-- | Show details of the current room.
look :: MonadMud m => PlayerId -> m ()
look pId = do
  w <- (^. gsWorld) <$> get
  (_, location) <- maybeThrow InternalError (M.lookup pId (w ^. wPlayers))
  reply ("# " <> locName location)
  reply (description location)
  reply ("Exits: " <> showExits (exits location))
  reply ("Players here: " <> pack (show (showPlayer <$> (playersAtLocation location w))))
  where
    showExits = pack . show . (fst <$>) . M.toList

-- | Display a helpful message listing the commands available.
help :: MonadMud m => m ()
help = reply "The following commands are available: who, look, go <direction>, help, attack <target>, logout, whisper <player> <message>"

-- | Have the given player attack the target
attack :: MonadMud m => PlayerId -> String -> m ()
attack pId target = do
  w <- (^. gsWorld) <$> get
  case M.lookup pId (w ^. wPlayers) of
    Nothing -> clientError InternalError
    Just (p, loc) -> getLocation pId >>= \case
      Nothing -> clientError InternalError
      Just myLoc ->
        case targetPlayer target w of
          Nothing -> clientError (AttackError TargetNotFound)
          Just (t, tLoc) -> do
            if (locName tLoc == locName myLoc)
              then do
                case strike p t of
                  Just t' -> do
                    modify (over (gsWorld . wPlayers) (M.insert (t ^. playerId) (t', tLoc)))
                    name <- fromMaybe "UNKNOWN" <$> getPlayerName pId
                    sendToPlayer (t ^. playerId) (Message $ "You've been attacked by " <> pack name)
                  Nothing -> do
                    sendToPlayer (t ^. playerId) Disconnect
                    removePlayer (t ^. playerId)
              else clientError (AttackError TargetNotNear)

-- | Remove the given playre from the world
removePlayer :: MonadMud m => PlayerId -> m ()
removePlayer pId = modify (over (gsWorld . wPlayers) (sans pId))

-- | Get the location of the given player
getLocation :: MonadMud m => PlayerId -> m (Maybe Location)
getLocation pId = do
  players <- (^. gsWorld . wPlayers) <$> get
  return (snd <$> (M.lookup pId players))

-- | Get the name of the given player
getPlayerName :: MonadMud m => PlayerId -> m (Maybe String)
getPlayerName pId = do
  players <- (^. gsWorld . wPlayers) <$> get
  return ((^. pName) . fst <$> (M.lookup pId players))

-- | Send an event to the current client (the current client is the
-- one which sent the message being responded to)
sendToCurrentClient :: (MonadReader ClientId m, MonadWriter Messages m) => OutputEvent -> m ()
sendToCurrentClient ev = do
  cId <- ask
  tell [(cId, ev)]

-- | Send an event to the given player
sendToPlayer :: MonadMud m => PlayerId -> OutputEvent -> m ()
sendToPlayer targetId ev = do
  gs <- get
  let w = gs ^. gsWorld
  case getClientId gs targetId of
    Nothing -> clientError InternalError
    Just cId -> tell [(cId, ev)]

-- | Reply to the current client with a text message
reply :: (MonadReader ClientId m, MonadWriter Messages m) => Text -> m ()
reply = sendToCurrentClient . Message

-- | Send an error to the current client
clientError :: (MonadReader ClientId m, MonadWriter Messages m) => MudError -> m ()
clientError = sendToCurrentClient . OutputError
