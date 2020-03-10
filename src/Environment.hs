{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Environment where

import Import
import qualified RIO.Map as Map
import System.Environment (lookupEnv)

import Stage
import RIO.Process

portFromEnv :: IO Int
portFromEnv = do
 maybePort <- lookupEnv "PORT"
 let port = maybePort >>= readMaybe
 return $ fromMaybe 8080 port

stageFromEnv :: IO Stage
stageFromEnv = do
  maybeStage <- lookupEnv "STAGE"
  let stage = maybeStage >>= readMaybe
  return $ fromMaybe Dev stage

databaseUrlFromEnv :: IO String
databaseUrlFromEnv = do
  maybeUrl <- lookupEnv "DATABASE_URL"
  return $ fromMaybe "postgresql://postgres@localhost" maybeUrl
  
databaseUrlFromProcessContext :: ProcessContext -> DatabaseUrl
databaseUrlFromProcessContext pc =
   DatabaseUrl . encodeUtf8 . fromMaybe "postgresql://postgres@localhost" $ maybeUrl
   where 
     maybeUrl = Map.lookup "DATABASE_URL" envVars
     envVars = view envVarsL pc