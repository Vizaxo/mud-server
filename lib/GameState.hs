module GameState where

import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Control.Lens
import Control.Monad.State

import Event
import Player
import World

data GameState = GameState
  { _gsWorld :: World
  , _gsPlayers :: Map ClientId ClientState
  , _gsNextPlayerId :: Int
  }
  deriving (Show, Read)

data ClientState = EnteringName | InGame PlayerId
  deriving (Show, Read)
makePrisms ''ClientState

newGameState :: GameState
newGameState = GameState emptyWorld M.empty 0

-- | Use a lens to expand the scope of a stateful computation to a larger state
overState :: MonadState s m => Lens' s t -> State t a -> m a
overState lens st = do
  s <- get
  let (x, t') = runState st (view lens s)
  put (set lens t' s)
  return x

makeLenses ''GameState

-- | Generate a fresh player ID
freshPId :: MonadState GameState m => m PlayerId
freshPId = overState gsNextPlayerId $ get <* modify (+1)

getClientId :: GameState -> PlayerId -> Maybe ClientId
getClientId gs pId = listToMaybe $ mapMaybe filterPId $ M.toList (gs ^. gsPlayers)
  where
    filterPId (cId, InGame pId') | pId == pId' = Just cId
    filterPId _                                = Nothing
