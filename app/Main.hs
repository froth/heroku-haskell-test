{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Import
import RIO.Process
import Lib

main :: IO ()
main = do
  logOptions <- logOptionsHandle stderr True
  pc <- mkDefaultProcessContext
  withLogFunc logOptions $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          }
     in runRIO app startApp

