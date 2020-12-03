module Day02 where

import qualified Data.List as L
import Data.List.Split (divvy)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Lib (run)

runDay02Part1 :: IO String
runDay02Part1 = run "Day02-input.txt" part1

runDay02Part2 :: IO String
runDay02Part2 = run "Day02-input.txt" part2

-- runDay02Part2 :: IO String
-- runDay02Part2 = run "Day02-input.txt" part2
s = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]

s1 = "vtrphonkulbfejcyzmsqgdxpaw"

s2 = "vtnshorxuvbfejcyzmsqgdxpaw"

countOcc :: String -> (M.Map Char Integer, String)
countOcc s = (L.foldr (\x acc -> M.insertWith (+) x 1 acc) M.empty $ s, s)

hasNumOcc :: Integer -> (M.Map Char Integer, String) -> Bool
hasNumOcc n (m, s) = (length $ (M.filter (== n) m)) > 0

numLetters :: Integer -> [String] -> Integer
numLetters n = fromIntegral . length . L.filter (hasNumOcc n . countOcc)

part1 :: [String] -> Integer
part1 s = numLetters 2 s * numLetters 3 s

pairs :: [String] -> [(String, String)]
pairs a = [(x, y) | (x:xs) <- L.tails a, y <- xs]

commonLettersOneOff :: (String, String) -> Maybe String
commonLettersOneOff (x, y) =
    let common = filter (\t -> fst t == snd t) $ zip x y
        oneOff = length x - length common <= 1
     in if oneOff
            then Just . map fst $ common
            else Nothing

part2 :: [String] -> String
part2 s = head . catMaybes . map commonLettersOneOff . pairs $ s
