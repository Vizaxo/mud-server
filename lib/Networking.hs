module Networking where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Writer hiding (listen)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Monoid

import qualified Data.ByteString.Char8 as CBS
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

-- | Setup a socket ready for listening on the given port.
setupSocket :: Int -> IO Socket
setupSocket port = do
  addr <- resolve (show port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  setCloseOnExecIfNeeded (fdSocket sock)
  listen sock 5
  return sock
  where
    resolve port = do
      let hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
      addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
      return addr

write :: MonadIO m => Socket -> String -> m ()
write sock = void . liftIO . send sock . CBS.pack

receive :: (MonadPlus m, MonadIO m) => Socket -> m String
receive sock = do
  --TODO: recieve an unlimited number of bytes
  msg <- liftIO $ recv sock 1024
  when (CBS.length msg == 0) mzero
  return (init . init . CBS.unpack $ msg)
