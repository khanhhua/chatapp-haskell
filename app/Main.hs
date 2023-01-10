{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Network.Socket
import Control.Monad.State

import Data
import Networking (openListenSocket, readSocket)
import Control.Concurrent (Chan, newChan, forkIO, readChan, writeChan)

main :: IO ()
main = do
  socket <- openListenSocket 4000 
  serve socket


serve :: Socket -> IO ()
serve socket = do
  chanCmd :: Chan Cmd <- newChan
  forkIO $ do
    evalStateT (runApp chanCmd) []

  forever $ do
    (clientSocket, _) <- liftIO $ accept socket
    forkIO $ runClient clientSocket chanCmd


runApp :: Chan Cmd -> StateT [Roomname] IO ()
runApp chanCmd = do
  cmd <- liftIO $ readChan chanCmd

  case cmd of
    Login username ->
      liftIO $ putStrLn $ "Logging as " <> username
    Create roomname -> do 
      liftIO $ putStrLn $ "Creating new room as " <> roomname
      modify' (\x -> roomname : x)
    Send rooname message -> do
      pure ()
      -- modify' roomname :
    
  runApp chanCmd

runClient :: Socket -> Chan Cmd -> IO ()
runClient socket chanCmd =
  forever $ readSocket socket >>= writeChan chanCmd . read . rtrim


rtrim :: String -> String
rtrim = takeWhile (/= '\n')