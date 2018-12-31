module Day05 where

import Data.Char (isUpper, isLower, toUpper)
import Lib (run)
import Data.List (foldl')

e = "dabAcCaCBAcCcaDA"

reduce :: String -> String
reduce xs = let r = reduceOnce xs in if length r == length xs then r else reduce r

reduceOnce :: String -> String
reduceOnce = foldl' reduceStep []

reduceStep :: String -> Char -> String
reduceStep [] c = [c]
reduceStep (x:xs) c = if isOpp x c then xs else c:x:xs

isOpp :: Char -> Char -> Bool
isOpp a b = isUpper a /= isUpper b && (toUpper a == toUpper b)

part1 :: [String] -> Integer
part1 = fromIntegral . length . reduce . head

runDay05Part1 :: IO String
runDay05Part1 = run "Day05-input.txt" part1

remove :: Char -> String -> String
remove c = filter (\x -> c /= x && c /= toUpper x)

toRemove :: [Char]
toRemove = ['A' .. 'Z']

reducedSize :: String -> Char -> Integer
reducedSize s c = fromIntegral . length . reduce . remove c $ s

reducedSizes :: String -> Integer
reducedSizes s = minimum . map (reducedSize s) $ ['A' .. 'Z']

part2 :: [String] -> Integer
part2 = reducedSizes . reduce . head

runDay05Part2 :: IO String
runDay05Part2 = run "Day05-input.txt" part2
