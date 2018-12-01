import Day01 (runDay01Part1, runDay01Part2)
import Day02 (runDay02Part1, runDay02Part2)
import Day03 (runDay03Part1, runDay03Part2)
import Day04 (runDay04Part1, runDay04Part2)
import Day05 (runDay05Part1, runDay05Part2)
import Day06 (runDay06Part1)
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
            it "Part 2" $ do
                actual <- runDay03Part2
                actual `shouldBe` "658"
        describe "Day 04" $ do
            it "Part 1" $ do
                actual <- runDay04Part1
                actual `shouldBe` "101262"
            it "Part 2" $ do
                actual <- runDay04Part2
                actual `shouldBe` "71976"
        describe "Day 05" $ do
            it "Part 1" $ do
                actual <- runDay05Part1
                actual `shouldBe` "10888"
            it "Part 2" $ do
                actual <- runDay05Part2
                actual `shouldBe` "6952"
        describe "Day 06" $ do
            it "Part 1" $ do
                actual <- runDay06Part1
                actual `shouldBe` "5333"
