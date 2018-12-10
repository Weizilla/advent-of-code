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

symDiff :: (Ord a) => M.Map a b -> M.Map a b -> M.Map a b
symDiff a b = M.union (M.difference a b) (M.difference b a)

oneApart :: [String]
oneApart [(a, as), (b, bs)] =
    case (M.toList $ symDiff a b) of
        [(c, 1), (d, 1)] -> Just $ as ++ " " ++ bs
        _ -> Nothing

printM :: M.Map Char Integer -> String
printM m = L.concat . map (\(k, v) -> L.genericReplicate v k) $ M.toList m

part2 :: [String] -> String
part2 s = show result
  where
    result = catMaybes oneAparts
    oneAparts = map oneApart pairs
    pairs = divvy 2 1 occs
    occs = L.sort . map countOcc $ s
