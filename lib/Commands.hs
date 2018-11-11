module Commands where

import World

data Command
  = Who
  | Look
  | Go Direction
  | Help
  | Attack String
  deriving Show
