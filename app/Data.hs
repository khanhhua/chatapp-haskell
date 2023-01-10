{-# LANGUAGE InstanceSigs #-}
module Data where

import Data.List (intercalate)

type Username = String
type Roomname = String

data Cmd
  = Login Username
  | Create Roomname
  | Send Roomname String
  deriving (Show)

instance Read Cmd where
  readsPrec :: Int -> ReadS Cmd
  readsPrec _ input =
    case words input of
      ["login", username] -> [(Login username, "")]
      ["create", roomname] -> [(Create roomname, "")]
      "send" : roomname : more -> [(Send roomname $ unwords more, "")]
      _ -> []

data Client = Client
  { username :: Username
  , online :: Bool
  }


data Chatroom = Chatroom
  { name :: String
  , users :: [Username]
  }