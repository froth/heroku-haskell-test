{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import RIO

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
newtype DatabaseUrl = DatabaseUrl ByteString

data Env = Env
  { appLogFunc :: !LogFunc
  , connectionPool :: !(Pool Connection)
  -- Add other app-specific configuration information here
  }

instance HasLogFunc Env where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

class WithConnectionPool env where
  connectionPoolL :: Lens' env (Pool Connection)

instance WithConnectionPool Env where
  connectionPoolL = lens connectionPool (\x y -> x {connectionPool = y})