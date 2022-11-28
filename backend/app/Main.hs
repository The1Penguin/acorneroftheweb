{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.Wai.Middleware.RequestLogger
import           Prelude
import           Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  middleware logStdout

  get "/" $ do
    json $ (undefined :: String)
