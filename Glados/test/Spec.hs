import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Example Spec" $ do
    it "True should be True" $ do
      True `shouldBe` True
    it "1 + 1 should be 2" $ do
      1 + 1 `shouldBe` 2
