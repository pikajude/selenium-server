selenium-server
===============
This library makes it a little easier to deal with selenium-based integration tests. It's designed to augment the existing [webdriver](http://hackage.haskell.org/package/webdriver) library.

Usage:

``` haskell
import Test.Hspec
import Test.WebDriver
import Test.WebDriver.Server

main :: IO ()
main = hspec $
    describe "My website" $
        it "is called joelt.io" $ do
            title <- withServer defaultSettings $
                runSession defaultSession defaultCaps $ do
                    openPage "https://joelt.io"
                    waitUntil 2000 getTitle
            title `shouldBe` "joelt.io"
```
