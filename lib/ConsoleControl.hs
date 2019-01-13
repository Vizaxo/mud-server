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
import LoadSave
import Utils

data ConsoleEvent = ConsoleMessage String
  deriving Show

data ConsoleCommand = Save | SaveAndQuit | Quit
  deriving Show

data ConsoleProcess
  = IOAction (forall m. (MonadIO m, MonadReader GameState m) => m ())
  | ConsoleOutputMessage String

instance Show ConsoleProcess where
  show (IOAction _) = "IOAction <function>"
  show (ConsoleOutputMessage msg) = "ConsoleOutputMessage " <> show msg

consoleCommand :: Parser ConsoleCommand
consoleCommand = asum (mkSimple <$> [Save, SaveAndQuit, Quit]) <* eof

processConsoleEvent :: ConsoleEvent -> [ConsoleProcess]
processConsoleEvent (ConsoleMessage msg) = case parse consoleCommand "" msg of
  Left e -> [ConsoleOutputMessage $ "Error parsing command " <> msg]
  Right cmd -> case cmd of
    Save -> [IOAction save]
    SaveAndQuit -> [IOAction (save >> quit)]
    Quit -> [IOAction quit]

handleConsoleProcess :: MonadIO m => GameState -> ConsoleProcess -> m ()
handleConsoleProcess gs (IOAction fm) = runReaderT fm gs
handleConsoleProcess gs (ConsoleOutputMessage msg) = liftIO $ putStrLn (msg <> "\n")

save :: (MonadIO m, MonadReader GameState m) => m ()
save = (liftIO $ putStrLn "saving") >> (saveGame =<< ask)

quit :: MonadIO m => m ()
quit = liftIO exitSuccess
