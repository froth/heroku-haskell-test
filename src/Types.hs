{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import RIO

import Stage
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Lens.Micro.TH

newtype DatabaseUrl = DatabaseUrl ByteString

data Env = Env
  { _appLogFunc :: !LogFunc
  , _connectionPool :: !(Pool Connection)
  , _stage :: !Stage
  , _port :: !Int
  -- Add other app-specific configuration information here
  }

makeLenses ''Env

instance HasLogFunc Env where
  logFuncL = appLogFunc

class WithConnectionPool env where
  connectionPoolL :: Lens' env (Pool Connection)

instance WithConnectionPool Env where
  connectionPoolL = connectionPool

class WithStage env where
  stageL :: Lens' env Stage

instance WithStage Env where
  stageL = stage

class WithPort env where
  portL :: Lens' env Int

instance WithPort Env where
  portL = port