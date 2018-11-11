module Player where

import World

data Player = Player
  { pos :: Location
  , name :: String
  }
  deriving Show
