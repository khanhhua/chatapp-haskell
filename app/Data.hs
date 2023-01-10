{-# LANGUAGE InstanceSigs #-}
module Data where

import Data.List (intercalate)
import Control.Concurrent (MVar)

type ClientId = Int
type Username = String
type Roomname = String

data Cmd
  = Login Username
  | Create Roomname
  | ListRooms
  | Join Roomname
  | Send String
  deriving (Show)

instance Read Cmd where
  readsPrec :: Int -> ReadS Cmd
  readsPrec _ input =
    case words input of
      [":login", username] -> [(Login username, "")]
      [":create", roomname] -> [(Create roomname, "")]
      [":listrooms"] -> [(ListRooms, "")]
      [":join", roomname] -> [(Join roomname, "")]
      message -> [(Send $ unwords message, "")]

type SyncCmd = (ClientId, MVar String, Cmd)

data Client = Client
  { username :: Username
  , online :: Bool
  }


data Chatroom = Chatroom
  { name :: String
  , users :: [(ClientId, MVar String)]
  }

instance Show Chatroom where
  show (Chatroom name _) = name
