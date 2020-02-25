{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Lib
    ( startApp
    ) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple
import Servant
import qualified Data.ByteString.Char8 as B

import WaiHelpers
import Stage
import Env
import Control.Monad.IO.Class (liftIO)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Generic, Eq, Show, FromJSON, ToJSON)


newtype Foo = Foo
  {
    bar :: Int
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

type API = "pgtest" :> Get '[JSON] Foo :<|> "users" :> Get '[JSON] [User] :<|> Raw

startApp :: IO ()
startApp = do
  port <- portFromEnv
  env <- stageFromEnv
  databaseUrl <- databaseUrlFromEnv
  putStrLn $ "Stage: " ++ show env
  run port $ app (B.pack databaseUrl) env

app :: B.ByteString -> Stage -> Application
app url stage = sslRedirect stage . corsMiddleware $ serve api $ server url


api :: Proxy API
api = Proxy
server :: B.ByteString -> Server API
server url = liftIO (pgtest url) :<|> return users :<|> serveDirectoryWith (staticSettings "react")
users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , User 3 "Stephen" "Hawking"
        ]

pgtest :: B.ByteString -> IO Foo
pgtest url = do
  conn <- connectPostgreSQL url
  [Only i] <- query_ conn "select 2 + 2"
  return (Foo i)
