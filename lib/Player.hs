module Player where

import Stats

import Control.Lens
import GHC.Generics

data Player = Player
  { _pName :: String
  , _pStats :: Stats
  }
  deriving (Eq, Ord, Show, Generic)
makeLenses ''Player

mkPlayer :: String -> Player
mkPlayer name = Player name defaultStats
