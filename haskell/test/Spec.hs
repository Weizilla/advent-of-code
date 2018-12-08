import Day01
import Test.Hspec

main :: IO ()
main =
    hspec $ do
        describe "Day 01" $ do
            it "Part 1" $ do
                actual <- runDay01 part1
                actual `shouldBe` 513
            it "Part 2" $ do
                actual <- runDay01 part2
                actual `shouldBe` 287
