import Test.Hspec
import Test.WebDriver.Server

main :: IO ()
main = hspec $
    describe "The server" $
        it "runs" $ do
            v <- withServer defaultSettings (return ())
            v `shouldBe` ()
