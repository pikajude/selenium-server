{-# LANGUAGE ScopedTypeVariables #-}

module Test.WebDriver.Server.Poll where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Network

waitForServer :: IO ()
waitForServer = void $ go 0 where
    go x | x >= (30 :: Int) = error "Timed out after 30 seconds waiting for selenium server."
    go n = do
        threadDelay 1000000
        connectTo "127.0.0.1" (PortNumber 4444)
            `catch` (\(_ :: IOException) -> go (n + 1))
