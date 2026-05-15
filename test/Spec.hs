import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Relude.reverse" $ do
    it "reverse (reverse xs) == xs" $ do
      reverse (reverse [1 .. 10]) `shouldBe` ([1 .. 10] :: [Int])
