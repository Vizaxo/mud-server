module World where

import Player

import Control.Lens
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Map as M

data Location = Location
  { locName :: Text
  , description :: Text
  , exits :: Map Direction Location
  }

instance Show Location where
  show (Location name desc exits) = "Location " <> show name <> " " <> show desc

data Direction = North | South | East | West
  deriving (Eq, Ord, Show)

data World = World
  { _wPlayers :: Map PlayerId (Player, Location)
  }
makeLenses ''World

addPlayer :: Player -> Location -> World -> World
addPlayer p l (World ps) = World (M.insert (p ^. playerId) (p, l) ps)

movePlayer :: Player -> Direction -> World -> Maybe World
movePlayer p dir w@(World ps) = do
  (_, l) <- M.lookup (p ^. playerId) ps
  l' <- M.lookup dir (exits l)
  return $ set (wPlayers . ix (p ^. playerId) . _2) l' w

emptyWorld :: World
emptyWorld = World M.empty

spawn :: Location
spawn = Location "spawn" "You are at the spawn" (M.fromList [(North, castle)])

castle :: Location
castle = Location "castle" "You are in front of a large castle." (M.fromList [(South, spawn)])

targetPlayer :: String -> World -> Maybe (Player, Location)
targetPlayer name w = lookup name $ (\(p, l) -> (p ^. pName, (p, l))) <$> M.elems (w ^. wPlayers)

playersAtLocation :: Location -> World -> [Player]
playersAtLocation loc w = w ^. wPlayers & M.elems & filter ((== locName loc) . locName . snd) & map fst
