module NPC where

import Stats

import Control.Lens
import GHC.Generics

data NPC = NPC
  { _npcName :: String
  , _npcStats :: Stats
  }
  deriving (Show, Generic)
makeLenses ''NPC
