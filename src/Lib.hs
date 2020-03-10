{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( startApp
    ) where
    
import Import
import Data.Aeson
import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple
import Servant
import qualified RIO.ByteString as B

import WaiHelpers
import Stage
import Environment
import Data.Pool (withResource)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Generic, Eq, Show, FromJSON, ToJSON)


newtype Foo = Foo
  {
    bar :: Int
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

type API = "pgtest" :> Get '[JSON] Foo -- :<|> "users" :> Get '[JSON] [User] :<|> Raw

startApp :: RIO Env ()
startApp = do
  port <- liftIO portFromEnv
  stage <- liftIO stageFromEnv
  logInfo $ "Stage: " <> displayShow stage
  application <- app
  liftIO $ run port application

app :: WithConnectionPool env => RIO env Application
--TODO stage sslRedirect Prod . corsMiddleware $ e
app =
   fmap (serve api) server


api :: Proxy API
api = Proxy
server :: WithConnectionPool env => (RIO env (Server API))
server =
 fmap return pgtest -- :<|> return users :<|> return serveDirectoryWith (staticSettings "react")

users :: RIO env [User]
users = return [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , User 3 "Stephen" "Hawking"
        ]

pgtest :: WithConnectionPool env => RIO env Foo
pgtest = do
  env <- ask
  let conns = view connectionPoolL env
  liftIO (executePg conns)
  where
    executePg conns = withResource conns $ \conn -> do
      [Only i] <- query_ conn "select 2 + 2"
      return (Foo i)
