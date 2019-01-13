module Parser (command, playerName) where

import Commands
import World

import Control.Applicative (some)
import Data.Char
import Data.Foldable
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char


command :: Parser Command
command = (try simpleCommand <|> try go <|> try attack <|> whisper) <* eof

simpleCommand = asum (mkSimple <$> [Who, Look, Help, Logout])

-- | Make a parser from a constructor, expecting the @show@
-- representation of it (case-insensitive).
mkSimple :: Show a => a -> Parser a
mkSimple x = x <$ string (map toLower (show x))

go :: Parser Command
go = Go <$> (string "go" *> some space *> direction)

direction :: Parser Direction
direction = north <|> south <|> east <|> west
  where
    north = North <$ string "north"
    south = South <$ string "south"
    east  = East <$ string "east"
    west  = West <$ string "west"

attack :: Parser Command
attack = Attack <$> (string "attack" *> some space *> playerName)

whisper :: Parser Command
whisper = Whisper <$> (string "whisper" *> some space *> playerName) <*> many anyChar

playerName :: Parser String
playerName = (:) <$> letter <*> many alphaNum
