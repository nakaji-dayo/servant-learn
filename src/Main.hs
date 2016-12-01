{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Aeson
import GHC.Generics

type UserAPI = "users" :>  Get '[JSON] [User]
               :<|> "albert" :> Get '[JSON] User
               :<|> "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
               :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
               :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Sort = Age | Name

data User = User {
    name :: String
    , age:: Int
} deriving (Eq, Show, Generic)

instance ToJSON User

data Position = Position {
    xCoord :: Int
    , yCood :: Int
} deriving (Generic)

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
                     deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo {
    clientName :: String
    , clientEmail :: String
    , clientAge :: Int
} deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
             { from :: String
             , to :: String
             , subject :: String
             , body :: String
             } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email "test@pig-brewing.com" (clientEmail c) "hello" "test email"

isaac :: User
isaac = User "Isaac Newton" 372

users1 :: [User]
users1 = [
    User "Daishi Nakajima" 28
    , User "Piyo" 30
    , User "Piyo Piyo" 30
    , isaac
    ]

server :: Server UserAPI
server = return users1 :<|> return isaac :<|> position :<|> hello :<|> marketing
  where
    position :: Int -> Int -> Handler Position
    position x y = return $ Position x y
    hello :: Maybe String -> Handler HelloMessage
    hello (Just name) = return $ HelloMessage ("Hello, " ++ name)
    hello Nothing = return $ HelloMessage "Who are you ?"
    marketing :: ClientInfo -> Handler Email
    marketing c = return $ emailForClient c

userAPI :: Proxy UserAPI
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server

main :: IO ()
main = run 8081 app1
