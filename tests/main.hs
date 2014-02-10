{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
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
                    openPage "http://www.baidu.com"
                    searchBox <- findElem (ByName "wd")
                    sendKeys "Cheese!" searchBox
                    submit searchBox
                    setImplicitWait 50
                    waitUntil 2000 getTitle
            T.take 7 v `shouldBe` "Cheese!"
