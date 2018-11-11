module World where

import Player

data Location
  = Spawn
  | Castle
  | Forest
  deriving Show

data World = World
  { players :: [(Player, Location)]
  }

data Direction = North | South | East | West
  deriving Show

addPlayer :: Player -> Location -> World -> World
addPlayer p l (World ps) = World ((p, l):ps)
