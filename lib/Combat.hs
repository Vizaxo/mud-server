module Combat where

import Stats

import Control.Lens
import Data.Generics.Product

data CombatError
  = TargetNotFound
  | TargetNotNear
  deriving Show

strike :: (HasType Stats a, HasType Stats d) => a -> d -> Maybe d
strike attacker defender
  = let atk = attacker ^. getStat sStrength
        def = defender ^. getStat sDefence
        damage = max 0 (atk - def)
    in maybeDead (over (getStat sHitpoints) (subtract damage) defender)

maybeDead :: HasType Stats d => d -> Maybe d
maybeDead entity = if entity ^. getStat sHitpoints <= 0 then Nothing else Just entity
