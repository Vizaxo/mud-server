module Parser (command) where

import Commands
import World

import Text.Parsec
import Text.Parsec.String

command :: Parser Command
command = (who <|> look <|> go) <* eof

who :: Parser Command
who = Who <$ string "who"

look :: Parser Command
look = Look <$ string "look"

go :: Parser Command
go = Go <$> (string "go" *> many1 space *> direction)

direction :: Parser Direction
direction = north <|> south <|> east <|> west
  where
    north = North <$ string "north"
    south = South <$ string "south"
    east  = East <$ string "east"
    west  = West <$ string "west"
