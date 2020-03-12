{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Environment where

import Import
import qualified RIO.Map as Map
import System.Environment (lookupEnv)

import qualified RIO.Text as T
import Stage
import RIO.Process

portFromPC :: ProcessContext -> Int
portFromPC pc = 
  fromMaybe 8080 $ maybePort >>= (readMaybe . T.unpack)
  where 
    maybePort = envFromPC pc "PORT"

databaseUrlFromPC :: ProcessContext -> DatabaseUrl
databaseUrlFromPC pc =
   DatabaseUrl . encodeUtf8 . fromMaybe "postgresql://postgres@localhost" $ maybeUrl
   where 
     maybeUrl = envFromPC pc "DATABASE_URL"

stageFromPC :: ProcessContext -> Stage
stageFromPC pc =
   fromMaybe Dev $ maybeStage >>= (readMaybe . T.unpack)
   where 
     maybeStage = envFromPC pc "STAGE"
  
envFromPC :: ProcessContext -> Text -> Maybe Text
envFromPC pc key = Map.lookup key envVars
  where
    envVars = view envVarsL pc
