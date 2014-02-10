{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.WebDriver
import Test.WebDriver.Server
import Test.WebDriver.Commands.Wait

main :: IO ()
main = hspec $
    describe "The server" $
        it "runs" $ do
            v <- withServer defaultSettings $
                runSession defaultSession defaultCaps $ do
                    openPage "https://joelt.io"
                    link <- findElem (ByLinkText "stuff")
                    click link
                    waitUntil 2000 getTitle
            v `shouldBe` "joelt.io | everything"
