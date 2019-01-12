module Player where

import Stats

import Control.Lens
import Data.Monoid
import GHC.Generics

type PlayerId = Int
data Player = Player
  { _pId :: PlayerId
  , _pName :: String
  , _pStats :: Stats
  }
  deriving (Eq, Ord, Show, Generic)
makeLenses ''Player

showPlayer :: Player -> String
showPlayer p = (p ^. pName) <> " (" <> show (p ^. getStat sHitpoints) <> "hp)"
