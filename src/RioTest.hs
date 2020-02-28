module RioTest where

import Data.Text
import Control.Monad.Reader
import Stage
import Env

data Runner = Runner
  { runnerLog :: String -> IO () -- print a log message
  , more :: String
  }

data Config = Config
  { configRunner :: Runner
  , takeFromEnv :: Bool
  }

class HasConfig env where
  getConfig :: env -> Config

instance HasConfig Config where
  getConfig = id

class HasRunner env where
  getRunner :: env -> Runner

instance HasRunner Runner where
  getRunner = id

instance HasRunner Config where
  getRunner = configRunner


takesFromEnvMaybe :: (HasConfig env, MonadIO m) => ReaderT env m Stage
takesFromEnvMaybe = do
  config <- ask
  let x = readOrNot . takeFromEnv $ getConfig config
  liftIO x
  where
    readOrNot :: Bool -> IO Stage
    readOrNot bool = if bool then
      stageFromEnv
    else
      return Stage.Prod



doesLoggingAndMore :: (HasRunner env, MonadIO m) =>  String -> ReaderT env m ()
doesLoggingAndMore text = do
  env <- ask
  let logfunction = runnerLog $ getRunner env
  liftIO $ logfunction text

myRunner :: Runner
myRunner = Runner {runnerLog=putStrLn, more="string"}

myConfig :: Config
myConfig = Config {configRunner=myRunner, takeFromEnv=False}

main :: IO ()
main = runReaderT program myConfig
  where
    program :: ReaderT Config IO ()
    program = do
      doesLoggingAndMore "text"
      stage <- takesFromEnvMaybe
      doesLoggingAndMore $ show stage
      doesLoggingAndMore "anotherText"
