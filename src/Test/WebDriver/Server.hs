{-# LANGUAGE ScopedTypeVariables #-}

module Test.WebDriver.Server (
    -- *** Running the server
    withServer,
    defaultSettings,
    -- *** Configuring the server
    ServerLocation(..),
    ServerSettings(..)
) where

import Network
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
withServer :: ServerSettings -> IO () -> IO ()
withServer ss act = do
    jarfile <- case jarLocation ss of
                   Nothing -> Download.downloadJar (serverVersion ss)
                   Just f -> return f
    error $ show jarfile
