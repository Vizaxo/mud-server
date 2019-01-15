module Parser (command, playerName) where

import Commands
import Utils
import World

import Control.Applicative (some)
import Data.Char
import Data.Foldable
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char


command :: Parser Command
command = (try simpleCommand <|> try go <|> try attack <|> whisper) <* eof

simpleCommand :: Parser Command
simpleCommand = asum (mkSimple <$> [Who, Look, Help, Logout])

go :: Parser Command
go = Go <$> (string "go" *> some space *> direction)

direction :: Parser Direction
direction = asum (mkSimple <$> [North, South, East, West])

attack :: Parser Command
attack = Attack <$> (string "attack" *> some space *> playerName)

whisper :: Parser Command
whisper = Whisper <$> (string "whisper" *> some space *> playerName) <*> many anyChar

playerName :: Parser String
playerName = (:) <$> letter <*> many alphaNum
