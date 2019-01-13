module Commands where

import World

data Command
  = Who
  | Look
  | Go Direction
  | Help
  | Attack String
  | Logout
  | Whisper String String
  deriving Show
