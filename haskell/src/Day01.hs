module Day01 where

import Data.Char (isNumber)
import Data.List (foldl', scanl')
import qualified Data.Set as S
import Lib (run)
import System.IO

runDay01 :: (String -> Integer) -> IO Integer
runDay01 f = run "Day01-input.txt" f

part1 :: String -> Integer
part1 = sum . map parseLine . lines

part2 :: String -> Integer
part2 = findFirstDup S.empty . sums

sums :: String -> [Integer]
sums = scanl1 (+) . cycle . map parseLine . lines

parseLine :: String -> Integer
parseLine = read . filter (\c -> isNumber c || c == '-')

-- Pretty sure this is the pattern that State monad uses
acc :: (S.Set Integer, Maybe Integer)
    -> Integer
    -> (S.Set Integer, Maybe Integer)
acc (s, r@(Just _)) _ = (s, r)
acc (s, r@(Nothing)) x =
    if S.member x s
        then (ss, Just x)
        else (ss, r)
  where
    ss = S.insert x s

findFirstDup :: S.Set Integer -> [Integer] -> Integer
findFirstDup seen (x:xs) =
    if S.member x seen
        then x
        else findFirstDup newSeen xs
  where
    newSeen = S.insert x seen
