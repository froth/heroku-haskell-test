{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Import
import RIO.Process
import Lib
import Environment
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple

main :: IO ()
main = do
  logOptions <- logOptionsHandle stderr True
  pc <- mkDefaultProcessContext
  let dataBaseUrlFromEnv = databaseUrlFromPC pc
  let stageFromEnv = stageFromPC pc
  let portFromEnv = portFromPC pc
  myConnectionPool <- initConnectionPool dataBaseUrlFromEnv
  withLogFunc logOptions $ \lf ->
    let app = Env
          { appLogFunc = lf
          , connectionPool = myConnectionPool
          , stage = stageFromEnv
          , port = portFromEnv
          }
     in runRIO app startApp

initConnectionPool :: DatabaseUrl -> IO (Pool Connection)
initConnectionPool (DatabaseUrl connStr) = createPool (connectPostgreSQL connStr) close 2 60 5