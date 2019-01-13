module LoadSave where

import Control.Exception
import Control.Monad.Trans
import Text.Read

import GameState

data LoadSaveError
  = FileCorrupt

saveFilePath :: FilePath
saveFilePath = "world"

loadOrCreateSave :: IO (Either LoadSaveError GameState)
loadOrCreateSave = readFromFile `catch` const @_ @IOException defaultGameState
  where
    readFromFile =
      do saveFile <- readFile saveFilePath
         case readMaybe saveFile of
           Nothing -> return (Left FileCorrupt)
           Just gs -> return (Right gs)
    defaultGameState = return (Right newGameState)

saveGame :: MonadIO m => GameState -> m ()
saveGame = liftIO . writeFile saveFilePath . show
