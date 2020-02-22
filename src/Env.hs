module Env where

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Stage

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