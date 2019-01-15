module Event where

import Data.Text
import Network.Socket (Socket)
import Text.Parsec (ParseError (..))

import Combat

data InputEvent = Connected Socket | Sent String | Disconnected
  deriving Show

data MudError
  = CommandParseError ParseError
  | InternalError
  | AttackError CombatError
  | CantGoThatWay
  | InvalidName ParseError
  | WhisperTargetNotFound
  deriving Show

data OutputEvent
  = Message Text
  | OutputError MudError
  | Disconnect
  deriving Show

type ClientId = Int
type ClientEvent = (ClientId, InputEvent)
