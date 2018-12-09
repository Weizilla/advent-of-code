module Day02 where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Lib (run)

runDay02Part1 :: IO Integer
runDay02Part1 = run "Day02-input.txt" part1

--runDay02Part2 :: (String -> Integer) -> IO Integer
--runDay02Part2 f = run "Day02-input.txt" part2
s = "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab"

countOcc :: String -> M.Map Char Integer
countOcc = L.foldr (\x acc -> M.insertWith (+) x 1 acc) M.empty

hasNumOcc :: Integer -> M.Map Char Integer -> Bool
hasNumOcc n m = (length $ (M.filter (== n) m)) > 0

numLetters :: Integer -> [String] -> Integer
numLetters n = fromIntegral . length . L.filter (hasNumOcc n . countOcc)

part1 :: [String] -> Integer
part1 s = numLetters 2 s * numLetters 3 s
