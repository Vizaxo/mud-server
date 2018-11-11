module Combat where

import Stats

import Control.Lens
import Data.Generics.Product

data CombatError
  = TargetNotFound
  | TargetNotNear
  deriving Show

strike :: (HasType Stats a, HasType Stats d) => a -> d -> d
strike attacker defender
  = let atk = attacker ^. getStat sStrength
        def = defender ^. getStat sDefence
        damage = max 0 (atk - def)
    in over (typed @Stats . sHitpoints . sAdjusted) (subtract damage) defender
