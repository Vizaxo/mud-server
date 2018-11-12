module World where

import Player

import Control.Lens
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M

data Location = Location
  { locName :: String
  , description :: String
  , exits :: Map Direction Location
  }
  deriving Show

data Direction = North | South | East | West
  deriving (Eq, Ord, Show)

data World = World
  { _wPlayers :: Map String (Player, Location)
  }
makeLenses ''World

addPlayer :: Player -> Location -> World -> World
addPlayer p l (World ps) = World (M.insert (p ^. pName) (p, l) ps)

movePlayer :: Player -> Direction -> World -> Maybe World
movePlayer p dir w@(World ps) = do
  (_, l) <- M.lookup (p ^. pName) ps
  l' <- M.lookup dir (exits l)
  return $ set (wPlayers . ix (p ^. pName) . _2) l' w

emptyWorld :: World
emptyWorld = World M.empty

spawn :: Location
spawn = Location "spawn" "You are at the spawn" (M.fromList [(North, castle)])

castle :: Location
castle = Location "castle" "You are in front of a large castle." (M.fromList [(South, spawn)])

targetPlayer :: String -> Traversal' World (Player, Location)
targetPlayer name = wPlayers . ix name

playersAtLocation :: Location -> World -> [Player]
playersAtLocation loc w = w ^. wPlayers & M.elems & filter ((== locName loc) . locName . snd) & map fst
