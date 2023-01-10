{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (Chan, MVar, forkIO, newChan, newEmptyMVar, putMVar, readChan, takeMVar, writeChan)
import Control.Monad.State
import Data
import Data.List (find)
import Data.Maybe (isJust)
import Network.Socket
import Networking (openListenSocket, readSocket, sendSocket)

main :: IO ()
main = do
  socket <- openListenSocket 4000
  serve socket

serve :: Socket -> IO ()
serve socket = do
  chanCmd :: Chan SyncCmd <- newChan
  forkIO $ do
    evalStateT (runApp chanCmd) []

  let loop nextClientId = do
        (clientSocket, _) <- liftIO $ accept socket
        forkIO $ runClient nextClientId clientSocket chanCmd
        loop (nextClientId + 1)

  loop 1

runApp :: Chan SyncCmd -> StateT [Chatroom] IO ()
runApp chanCmd = do
  (clientId, returnMVar, cmd) <- liftIO $ readChan chanCmd

  case cmd of
    Login username ->
      liftIO $ putStrLn $ "Logging as " <> username
    Create roomname -> do
      liftIO $ putStrLn $ "Creating new room as " <> roomname
      modify' (\x -> Chatroom roomname [] : x)
    ListRooms -> do
      rooms <- get
      liftIO $ do
        putStrLn $ "List rooms " <> show rooms
        putMVar returnMVar $ show rooms
    Join roomname -> do
      chatrooms <- get
      let maybeRoom = find (\r -> name r == roomname) chatrooms
      case maybeRoom of
        Nothing -> pure ()
        Just chatroom ->
          put $
            map
              ( \r@Chatroom {users = users} ->
                  if name r == roomname
                    then r {users = (clientId, returnMVar) : users}
                    else r
              )
              chatrooms
    Send message -> do
      chatrooms <- get
      let maybeRoom =
            find
              ( \Chatroom {users = roomUsers} ->
                  isJust $ find (\(key, _) -> clientId == key) roomUsers
              )
              chatrooms
      case maybeRoom of
        Nothing -> pure ()
        Just (Chatroom _ roomUsers) ->
          liftIO $ mapM_ (\(recipientClientId, returnMVar) ->
            when (recipientClientId /= clientId) $ putMVar returnMVar message
            ) roomUsers

  runApp chanCmd

runClient :: ClientId -> Socket -> Chan SyncCmd -> IO ()
runClient clientId socket chanCmd = do
  returnMVar :: MVar String <- newEmptyMVar

  forkIO . forever $ do
    returned <- takeMVar returnMVar
    sendSocket socket $ returned <> "\n"
  forever $ readSocket socket >>= writeChan chanCmd . makeSyncCmd returnMVar . read . rtrim
  where
    makeSyncCmd :: MVar String -> Cmd -> SyncCmd
    makeSyncCmd mVar cmd = (clientId, mVar, cmd)

rtrim :: String -> String
rtrim = takeWhile (/= '\n')