{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Network.Wai.Middleware.Cors

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = do
  port <- getPortFromEnv
  run port app

getPortFromEnv :: IO Int
getPortFromEnv = do
 maybePort <- lookupEnv "PORT"
 let port = maybePort >>= readMaybe
 return $ fromMaybe 8080 port

app :: Application
app = corsMiddleware $ serve api server
  where corsMiddleware :: Middleware
        corsMiddleware = cors (const $ Just simpleCorsResourcePolicy{
          corsOrigins = Just(["https://froth-react-test.herokuapp.com"], True)
        })

api :: Proxy API
api = Proxy

server :: Server API
server = return users
users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , User 3 "Stephen" "Hawking"
        ]
