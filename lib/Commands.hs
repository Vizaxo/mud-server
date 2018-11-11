module Commands where

import World

data Command
  = Who
  | Look
  | Go Direction
  deriving Show
