module Networking where

import qualified Control.Exception as E
import Network.Socket
import Network.Socket.ByteString (recv)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 as UTF8
import Data.ByteString.Lazy.UTF8 as BLU (fromString)
import Network.Socket.ByteString.Lazy (send, sendAll)

openListenSocket :: Int -> IO Socket
openListenSocket port = resolve >>= open

  where
    resolve = do
      let hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
      head <$> getAddrInfo (Just hints) (Just "localhost") (Just $ show port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock port
      return sock


readSocket :: Socket -> IO String
readSocket socket = do
  bytes <- recv socket (1024 :: Int)
  return $ toString bytes

sendSocket :: Socket -> String -> IO ()
sendSocket socket = sendAll socket . BLU.fromString