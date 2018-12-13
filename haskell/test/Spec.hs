import Day01
import Day02
import Day03
import Test.Hspec

main :: IO ()
main =
    hspec $ do
        describe "Day 01" $ do
            it "Part 1" $ do
                actual <- runDay01Part1
                actual `shouldBe` "513"
            it "Part 2" $ do
                actual <- runDay01Part2
                actual `shouldBe` "287"
        describe "Day 02" $ do
            it "Part 1" $ do
                actual <- runDay02Part1
                actual `shouldBe` "3952"
            it "Part 2" $ do
                actual <- runDay02Part2
                actual `shouldBe` "\"vtnikorkulbfejvyznqgdxpaw\""
        describe "Day 03" $ do
            it "Part 1" $ do
                actual <- runDay03Part1
                actual `shouldBe` "105047"
