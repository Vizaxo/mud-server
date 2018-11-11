module Player where

import Stats

import Control.Lens
import Data.Monoid
import GHC.Generics

data Player = Player
  { _pName :: String
  , _pStats :: Stats
  }
  deriving (Eq, Ord, Show, Generic)
makeLenses ''Player

mkPlayer :: String -> Player
mkPlayer name = Player name defaultStats

showPlayer :: Player -> String
showPlayer p = (p ^. pName) <> " (" <> show (p ^. getStat sHitpoints) <> "hp)"
