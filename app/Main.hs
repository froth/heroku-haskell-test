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
  let dataBaseUrlFromEnv = databaseUrlFromProcessContext pc
  myConnectionPool <- initConnectionPool dataBaseUrlFromEnv
  withLogFunc logOptions $ \lf ->
    let app = Env
          { appLogFunc = lf
          , connectionPool = myConnectionPool
          }
     in runRIO app startApp

initConnectionPool :: DatabaseUrl -> IO (Pool Connection)
initConnectionPool (DatabaseUrl connStr) = createPool (connectPostgreSQL connStr) close 2 60 5