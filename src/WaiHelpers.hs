{-# LANGUAGE OverloadedStrings #-}
module WaiHelpers (corsMiddleware, sslRedirect, staticSettings) where

import Stage

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.ForceSSL
import Network.Wai.Application.Static
import WaiAppStatic.Types

corsMiddleware :: Middleware
corsMiddleware = cors
  (const $ Just simpleCorsResourcePolicy
    { corsOrigins = Just (["https://froth-react-test.herokuapp.com"], True)
    }
  )

sslRedirect :: Stage -> Middleware
sslRedirect Dev = id
sslRedirect Prod = forceSSL

staticSettings :: String -> StaticSettings
staticSettings path = ((defaultWebAppSettings path){ssIndices = [unsafeToPiece "index.html"]})