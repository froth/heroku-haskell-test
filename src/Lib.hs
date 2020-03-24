{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( startApp
    ) where

import Control.Monad.Except (ExceptT(..))
import Data.Aeson
import Data.Pool (withResource)
import Database.PostgreSQL.Simple
import Import
import Network.Wai.Handler.Warp
import Servant
import WaiHelpers

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Generic, Eq, Show)
    deriving anyclass (FromJSON, ToJSON)

newtype Foo = Foo
  {
    bar :: Int
  } deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

type API = "pgtest" :> Get '[JSON] Foo :<|> "users" :> Get '[JSON] [User] :<|> Raw

startApp :: RIO Env ()
startApp = do
  localPort <- view port
  theStage <- view stage
  logInfo $ "Stage: " <> displayShow theStage
  logInfo $ "port" <> displayShow localPort
  application <- buildApp
  liftIO $ run localPort application

buildApp :: (WithStage env, WithConnectionPool env) => RIO env Application
buildApp = do
  localstage <- view stageL
  env <- ask 
  return . sslRedirect localstage . corsMiddleware . serve api . hoist $ env

api :: Proxy API
api = Proxy

server :: WithConnectionPool env => ServerT API (RIO env)
server = pgtest :<|> users :<|> serveDirectoryWith (staticSettings "react")

users :: RIO env [User]
users = return [ User 1 "Isaac" "Newton"
               , User 2 "Albert" "Einstein"
               , User 3 "Stephen" "Hawking"
               ]

pgtest :: WithConnectionPool env => RIO env Foo
pgtest = do
  conns <- view connectionPoolL
  liftIO (executePg conns)
  where
    executePg conns = withResource conns $ \conn -> do
      [Only i] <- query_ conn "select 2 + 2"
      return (Foo i)

hoist :: forall env. WithConnectionPool env => env -> Server API
hoist env = hoistServer api nat server
  where nat :: RIO env a -> Servant.Handler a
        nat act = Servant.Handler $ ExceptT $ try $ runRIO env act -- TODO move this runRIO to Main?
