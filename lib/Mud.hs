module Mud where

import Control.Arrow
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Lens
import Data.Text
import Data.Map as M
import Data.Monoid
import Text.Parsec (parse, ParseError (..), eof)
import Network.Socket (Socket)

import Parser
import Player
import Stats
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
  | InvalidName
  | WhisperTargetNotFound
  deriving Show

data OutputEvent
  = Message Text
  | OutputError MudError
  | Disconnect
  deriving Show

type Messages = [(ClientId, OutputEvent)]

-- | A convenient type synonym for the monadic operations the Mud supports
type MonadMud m = (MonadReader ClientId m, MonadState GameState m, MonadWriter Messages m)

-- | The pure version of @processEvent'@, which is used in the FRP module.
processEvent :: (ClientId, InputEvent) -> GameState -> ([(ClientId, OutputEvent)], GameState)
processEvent (cId, ev) = runState $ flip runReaderT cId $ execWriterT $ processEvent' ev

-- | Process a client's message. This can update the game state,
-- and/or send messages to any of the clients.
processEvent' :: MonadMud m => InputEvent -> m ()
processEvent' (Connected sock) = welcomeMessage
processEvent' Disconnected = return ()
processEvent' (Sent msg) = do
  cId <- ask
  gs <- get
  case M.lookup cId (gs ^. gsPlayers) of
    Nothing -> clientError InternalError
    Just s -> case s of
      EnteringName -> do
        case parse (playerName <* eof) "" msg of
          Left e -> clientError InvalidName
          Right name -> do
            pId <- freshPId
            modify (over gsPlayers (M.insert cId (InGame pId)))
            modify (over (gsWorld . wPlayers) (M.insert pId (Player pId name defaultStats, spawn)))
      InGame pId -> case (parse command "" msg) of
        Left e -> clientError (CommandParseError e)
        Right c -> case c of
          Who -> who
          Look -> look pId
          Go dir -> do
            reply $ "Going " <> pack (show dir)
            let w = gs ^. gsWorld
            return ()
            case M.lookup pId (w ^. wPlayers) of
              Nothing -> clientError InternalError
              Just (p, l) -> case movePlayer p dir w of
                Nothing -> clientError CantGoThatWay
                Just w -> do
                  modify (set gsWorld w)
                  look pId
          Help -> help
          Attack target -> attack pId target
          Logout -> sendToCurrentClient Disconnect
          Whisper target whisperMsg -> do
            let w = gs ^. gsWorld
            case targetPlayer target w of
              Nothing -> clientError WhisperTargetNotFound
              Just (t, l) -> sendToPlayer (t ^. playerId) (Message $ "Message received: " <> pack whisperMsg)
  newline

newline :: MonadMud m => m ()
newline = reply ""

-- | Send a welcome message when a new client connects
welcomeMessage :: MonadMud m => m ()
welcomeMessage = do
  reply "Welcome to the MUD!"
  reply "What is your name?"
  cId <- ask
  modify (over gsPlayers (M.insert cId EnteringName))

-- | Send an event to the current client (the current client is the
-- one which sent the message being responded to)
sendToCurrentClient :: (MonadReader ClientId m, MonadWriter Messages m) => OutputEvent -> m ()
sendToCurrentClient ev = do
  cId <- ask
  tell [(cId, ev)]

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

who :: MonadMud m => m ()
who = do
  players <- (^. gsWorld . wPlayers) <$> get
  reply ("The following players are logged in: " <> (pack $ show $ view (_1 . pName) <$> M.elems players))

look :: MonadMud m => PlayerId -> m ()
look pId = do
  w <- (^. gsWorld) <$> get
  case M.lookup pId (w ^. wPlayers) of
    Nothing -> clientError InternalError
    Just (_, l) -> do
      reply (locName l)
      reply "---"
      reply (description l)
      reply ("Exits: " <> showExits (exits l))
      reply ("Players here: " <> pack (show (showPlayer <$> (playersAtLocation l w))))
  where
    showExits = pack . show . (fst <$>) . M.toList

help :: MonadMud m => m ()
help = reply "The following commands are available: who, look, go <direction>, help, attack <target>"

attack :: MonadMud m => PlayerId -> String -> m ()
attack pId target = do
  w <- (^. gsWorld) <$> get
  --TODO: better (monadic) error handling
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
                    sendToPlayer (t ^. playerId) (Message "you've been attacked")
                  Nothing -> do
                    sendToPlayer (t ^. playerId) Disconnect
                    modify (over (gsWorld . wPlayers) (sans (t ^. playerId)))
              else clientError (AttackError TargetNotNear)

getLocation :: MonadMud m => PlayerId -> m (Maybe Location)
getLocation pId = do
  players <- (^. gsWorld . wPlayers) <$> get
  return (snd <$> (M.lookup pId players))
