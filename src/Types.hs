{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import RIO

import Stage
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
newtype DatabaseUrl = DatabaseUrl ByteString

data Env = Env
  { appLogFunc :: !LogFunc
  , connectionPool :: !(Pool Connection)
  , stage :: !Stage
  , port :: !Int
  -- Add other app-specific configuration information here
  }

instance HasLogFunc Env where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

class WithConnectionPool env where
  connectionPoolL :: Lens' env (Pool Connection)

instance WithConnectionPool Env where
  connectionPoolL = lens connectionPool (\x y -> x {connectionPool = y})

class WithStage env where
  stageL :: Lens' env Stage

instance WithStage Env where
  stageL = lens stage (\x y -> x {stage = y})

class WithPort env where
  portL :: Lens' env Int

instance WithPort Env where
  portL = lens port (\x y -> x {port = y})