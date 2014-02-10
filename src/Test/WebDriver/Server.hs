{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.WebDriver.Server (
    -- *** Running the server
    withServer,
    defaultSettings,
    -- *** Configuring the server
    ServerLocation(..),
    ServerSettings(..)
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Network
import System.IO
import System.Process
import qualified Test.WebDriver.Server.Download as Download

-- | Where the server can be found. 'Remote' means @selenium-server@ won't be
-- started.
data ServerLocation = Local | Remote HostName PortID
                    deriving Show

-- | Server settings.
data ServerSettings = ServerSettings
                    { serverLocation :: ServerLocation
                    , jarLocation :: Maybe FilePath
                    , serverVersion :: Maybe String
                    } deriving Show

-- | Default server settings: start a local selenium instance, and
-- download the server jar if none is found.
defaultSettings :: ServerSettings
defaultSettings = ServerSettings Local Nothing Nothing

-- | Execute the given 'IO' action with a selenium server running.
withServer :: ServerSettings -> IO a -> IO a
withServer ss act = case serverLocation ss of
    Remote{} -> error "not handled yet"
    Local -> do
        jarfile <- case jarLocation ss of
                       Nothing -> Download.downloadJar (serverVersion ss)
                       Just f -> return f
        blackHole <- openFile
#ifdef WINDOWS
                         "NUL"
#else
                         "/dev/null"
#endif
                         AppendMode
        bracket (do (_,_,_,ch) <- createProcess
                        (proc "java" ["-jar", jarfile, "-port", "4444"])
                            { std_in = UseHandle blackHole
                            , std_out = UseHandle blackHole
                            , std_err = CreatePipe }
                    waitForServer
                    return ch)
                (\h -> terminateProcess h >> hClose blackHole)
                (const act)

waitForServer :: IO ()
waitForServer = void $ go 0 where
    go x | x >= (30 :: Int) = error "Timed out after 30 seconds waiting for selenium server."
    go n = do
        threadDelay 1000000
        connectTo "127.0.0.1" (PortNumber 4444)
            `catch` (\(_ :: IOException) -> go (n + 1))
