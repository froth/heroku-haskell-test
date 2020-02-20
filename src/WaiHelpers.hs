{-# LANGUAGE OverloadedStrings #-}
module WaiHelpers (corsMiddleware, sslRedirect) where

import Stage

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.ForceSSL

corsMiddleware :: Middleware
corsMiddleware = cors
  (const $ Just simpleCorsResourcePolicy
    { corsOrigins = Just (["https://froth-react-test.herokuapp.com"], True)
    }
  )

sslRedirect :: Stage -> Middleware
sslRedirect Dev = id
sslRedirect Prod = forceSSL
