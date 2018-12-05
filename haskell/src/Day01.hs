module Day01 where

import Data.Char (isNumber)
import Data.List (foldl', scanl')
import qualified Data.Set as S
import System.IO

inputFile = "Day01-input.txt"

sumLines :: String -> Integer
sumLines = sum . map parseLine . lines

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

day01a :: IO ()
day01a = do
    withFile
        inputFile
        ReadMode
        (\handle -> do
             contents <- hGetContents handle
             putStrLn . show . sumLines $ contents)

day01b :: IO ()
day01b = do
    withFile
        inputFile
        ReadMode
        (\handle -> do
             contents <- hGetContents handle
             putStrLn . show . findFirstDup S.empty . sums $ contents)
