module Event where

import Network.Socket (Socket)

data InputEvent = Connected Socket | Sent String | Disconnected
  deriving Show

type ClientId = Int
type ClientEvent = (ClientId, InputEvent)
