module World where

import Player

import Data.Map (Map)
import qualified Data.Map as M

data Location = Location
  { locName :: String
  , description :: String
  , exits :: Map Direction Location
  }
  deriving Show

data World = World
  { players :: Map Player Location
  }

data Direction = North | South | East | West
  deriving (Eq, Ord, Show)

addPlayer :: Player -> Location -> World -> World
addPlayer p l (World ps) = World (M.insert p l ps)

movePlayer :: Player -> Direction -> World -> World
movePlayer p dir w@(World ps) = case M.lookup p ps of
  Nothing -> w --TODO: error reporting
  Just (Location _ _ exits) -> case M.lookup dir exits of
    Nothing -> w
    Just l' -> World (M.insert p l' ps)

emptyWorld :: World
emptyWorld = World M.empty

spawn :: Location
spawn = Location "spawn" "You are at the spawn" (M.fromList [(North, castle)])

castle :: Location
castle = Location "castle" "You are in front of a large castle." (M.fromList [(South, spawn)])
