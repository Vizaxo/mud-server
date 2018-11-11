module Stats where

import Control.Lens
import Data.Generics.Sum
import Data.Generics.Product
import GHC.Generics

data Stat = Stat
  { _sBase :: Int
  , _sAdjusted :: Int
  }
  deriving (Eq, Ord, Show)
makeLenses ''Stat

data Stats = Stats
  { _sHitpoints :: Stat
  , _sStrength :: Stat
  , _sDefence :: Stat
  }
  deriving (Eq, Ord, Show, Generic)
makeLenses ''Stats

level :: Int -> Stat
level n = Stat n n

defaultStats :: Stats
defaultStats = Stats (level 10) (level 1) (level 0)

-- | Lens to a given stat, from anything that has stats.
getStat :: HasType Stats s => Lens' Stats Stat -> Lens' s Int
getStat stat = typed @Stats . stat . sAdjusted
