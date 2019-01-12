module Client where

import Control.Monad.State
import qualified Data.Map as M
import Data.Map (Map)
import Data.Monoid
import Network.Socket hiding (send, sendTo, recv, recvFrom, Connected)

import Event
import Networking
import Mud

type ClientPorts = Map ClientId Socket

emptyClientPorts = M.empty

getClientId :: MonadState ClientId m => m ClientId
getClientId = get <* modify (+1)

updateClients :: MonadState ClientPorts m => (ClientId, InputEvent) -> m ()
updateClients (id, Connected sock) = modify (M.insert id sock)
updateClients _ = return ()

sendToClient :: (MonadIO m) => ClientPorts -> (ClientId, OutputEvent) -> m ()
sendToClient ports (id, msg) = case M.lookup id ports of
  Nothing -> liftIO . print $ "Error sending message " <> show msg <> " to client " <> show id <> "\n" <> show ports
  Just socket -> write socket (show msg <> "\n")

runClientPorts :: Monad m => StateT ClientPorts m a -> m a
runClientPorts = flip evalStateT M.empty
