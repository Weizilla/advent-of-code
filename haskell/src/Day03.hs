module Day03 where

import Data.List as L
import qualified Data.Map as M
import Lib (run)
import Text.Regex.PCRE

e1 = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]

parseClaim :: String -> [(Integer, Integer)]
parseClaim input =
    let (_, _, _, r) =
            input =~ "#\\d+ @ (\\d+),(\\d+): (\\d+)x(\\d+)" :: ( String
                                                               , String
                                                               , String
                                                               , [String])
        ([l, t, w, h]) = map read r
     in [(x, y) | x <- [l .. (l + w - 1)], y <- [t .. (t + h - 1)]]

allClaims :: [String] -> [(Integer, Integer)]
allClaims = L.concat . map parseClaim

aggClaims :: [(Integer, Integer)] -> M.Map (Integer, Integer) Integer
aggClaims = foldr aggClaim M.empty

aggClaim ::
       (Integer, Integer)
    -> M.Map (Integer, Integer) Integer
    -> M.Map (Integer, Integer) Integer
aggClaim i acc = M.insertWith (+) i 1 acc

numOverlap :: M.Map (Integer, Integer) Integer -> Integer
numOverlap = fromIntegral . length . L.filter (> 1) . M.elems

part1 :: [String] -> Integer
part1 = numOverlap . aggClaims . allClaims

runDay03Part1 :: IO String
runDay03Part1 = run "Day03-input.txt" part1
