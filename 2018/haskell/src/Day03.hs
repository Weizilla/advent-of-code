module Day03 where

import Data.List as L
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Lib (run)
import Text.Regex.PCRE

e1 = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]

type ClaimMap = M.Map (Integer, Integer) Integer

data Claim = Claim
    { claimId :: Integer
    , points :: [(Integer, Integer)]
    } deriving (Show)

parseClaim :: String -> Claim
parseClaim input =
    let
        (_, _, _, r) =
            input =~ "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" :: (String, String, String, [String])
        [i, l, t, w, h] = map read r
    in Claim { claimId = i, points = [ (x, y) | x <- [l .. (l + w - 1)], y <- [t .. (t + h - 1)] ] }

allClaims :: [String] -> [Claim]
allClaims = map parseClaim

aggClaims :: [Claim] -> ClaimMap
aggClaims = L.foldl' aggClaim M.empty . concatMap points

aggClaim :: ClaimMap -> (Integer, Integer) -> ClaimMap
aggClaim acc p = M.insertWith (+) p 1 acc

numOverlap :: ClaimMap -> Integer
numOverlap = fromIntegral . length . L.filter (> 1) . M.elems

part1 :: [String] -> Integer
part1 = numOverlap . aggClaims . allClaims

runDay03Part1 :: IO String
runDay03Part1 = run "Day03-input.txt" part1

noOverlap :: ClaimMap -> Claim -> Maybe Claim
noOverlap m c =
    let
        onlyClaim = all (== 1) . map (m M.!) . points $ c
        result = if onlyClaim then Just c else Nothing
    in result

part2 :: [String] -> Integer
part2 xs =
    let
        claimMap = aggClaims all
        all = allClaims xs
        Claim { claimId = i } = head . mapMaybe (noOverlap claimMap) $ all
    in i

runDay03Part2 :: IO String
runDay03Part2 = run "Day03-input.txt" part2
