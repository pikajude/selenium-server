{-# LANGUAGE OverloadedStrings #-}

module Test.WebDriver.Server.Download (
    downloadJar
) where

import Data.ByteString.UTF8
import qualified Data.Conduit as C
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Downloader
import System.Directory
import System.FilePath
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString

downloadJar :: Maybe String -> IO FilePath
downloadJar Nothing = do
    latest <- latestJarVersion
    downloadJar (Just latest)

downloadJar (Just v) = do
    userdata <- getAppUserDataDirectory "selenium-hs"
    let fname = "selenium-server-standalone-" ++ v ++ ".jar"
        jarpath = userdata </> fname
    e <- doesFileExist jarpath
    if e
        then return jarpath
        else do
            createDirectoryIfMissing True userdata
            req <- parseUrl $ "http://selenium.googlecode.com/files/" ++ fname
            withManager $ \man -> do
                response <- http req man
                responseBody response C.$$+- sinkFile jarpath
            return jarpath

latestJarVersion :: IO String
latestJarVersion = do
    bod <- urlGetContents "http://code.google.com/p/selenium/downloads/list"
    let Right reg = compile defaultCompOpt defaultExecOpt
                        "selenium-server-standalone-([0-9]+\\.[0-9]+\\.[0-9]+)\\.jar"
        Right (Just (_,_,_,[vers])) = regexec reg bod
    return $ toString vers
