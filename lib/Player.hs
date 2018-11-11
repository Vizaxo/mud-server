module Player where

data Player = Player
  { pName :: String
  }
  deriving (Eq, Ord, Show)
