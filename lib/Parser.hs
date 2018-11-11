module Parser (command) where

import Commands
import World

import Data.Char
import Data.Foldable
import Text.Parsec
import Text.Parsec.String

command :: Parser Command
command = (simpleCommand <|> go) <* eof

simpleCommand = asum (mkSimple <$> [Who, Look, Help])

-- | Make a parser from a constructor, expecting the @show@
-- representation of it (case-insensitive).
mkSimple :: Show a => a -> Parser a
mkSimple x = x <$ ciString (show x)

go :: Parser Command
go = Go <$> (string "go" *> many1 space *> direction)

direction :: Parser Direction
direction = north <|> south <|> east <|> west
  where
    north = North <$ string "north"
    south = South <$ string "south"
    east  = East <$ string "east"
    west  = West <$ string "west"

-- | A case-insensitive version of the @string@ parser.
ciString :: String -> Parser String
ciString = traverse (\c -> char (toLower c) <|> char (toUpper c))
