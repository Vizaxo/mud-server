module ConsoleControl where

import Control.Applicative
import Control.Monad.Reader
import Data.Foldable
import Data.Monoid
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import System.Exit

import GameState
import Utils

data ConsoleEvent = ConsoleMessage String
  deriving Show

data ConsoleCommand = Quit
  deriving Show

data ConsoleProcess
  = IOAction (forall m. (MonadIO m, MonadReader GameState m) => m ())
  | ConsoleOutputMessage String

consoleCommand :: Parser ConsoleCommand
consoleCommand = asum (mkSimple <$> [Quit]) <* eof

processConsoleEvent :: ConsoleEvent -> [ConsoleProcess]
processConsoleEvent (ConsoleMessage msg) = case parse consoleCommand "" msg of
  Left e -> [ConsoleOutputMessage $ "Error parsing command " <> msg]
  Right cmd -> case cmd of
    Quit -> [IOAction quit]

handleConsoleProcess :: MonadIO m => GameState -> ConsoleProcess -> m ()
handleConsoleProcess gs (IOAction fm) = runReaderT fm gs
handleConsoleProcess gs (ConsoleOutputMessage msg) = liftIO $ putStrLn (msg <> "\n")

quit :: MonadIO m => m ()
quit = liftIO exitSuccess
