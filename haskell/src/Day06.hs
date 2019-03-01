module Day06 where

import qualified Data.Char as C
import qualified Data.Function as F
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Data.Set as S
import Lib (display, run)
import Text.Parsec (many, parse)
import Text.Parsec.Char (digit, string)
import Text.Parsec.String (Parser)

e = ["1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"]

data Point = Point
    { pX :: Integer
    , pY :: Integer
    } deriving (Show, Ord, Eq)

data Bounds = Bounds
    { maxX :: Integer
    , maxY :: Integer
    } deriving (Show)

type PointGrid = M.Map Point Char

parsePoint :: Parser Point
parsePoint = (\a b -> Point (read a) (read b)) <$> many digit <* string ", " <*> many digit

parseInput :: [String] -> [Point]
parseInput input =
    case mapM (parse parsePoint "") input of
        Right xs -> xs
        Left e -> error $ "Error parsing" ++ show e

calcBounds :: [Point] -> Bounds
calcBounds ps =
    let maxX = maximum . map pX $ ps
        maxY = maximum . map pY $ ps
     in Bounds maxX maxY

inBounds :: Point -> Bounds -> Bool
inBounds Point {pX = pX, pY = pY} Bounds {maxX = maxX, maxY = maxY} =
    or [pX == 0, pY == 0, pX == maxX, pY == maxY]

mDistance :: Point -> (Point, Char) -> (Integer, Char)
mDistance Point {pX = p2X, pY = p2Y} (Point {pX = p1X, pY = p1Y}, c) =
    (abs (p1X - p2X) + abs (p1Y - p2Y), c)

calcAllPoints :: Bounds -> [Point]
calcAllPoints Bounds {maxX = maxX, maxY = maxY} = [Point x y | y <- [0 .. maxY], x <- [0 .. maxX]]

calcNearest :: [Point] -> PointGrid -> PointGrid
calcNearest allPoints input =
    L.foldl' (\acc p -> M.insert p (calcPointChar p input) acc) M.empty allPoints

calcPointChar :: Point -> PointGrid -> Char
calcPointChar p g =
    let distances = map (mDistance p) $ M.toList g
        sorted = L.groupBy ((==) `F.on` fst) . L.sortOn fst $ distances
        minDists = head sorted
     in case (length minDists, head minDists) of
            (1, (0, c)) -> C.toUpper c
            (1, (_, c)) -> c
            otherwise -> '.'


calcInfiniteChars :: Bounds -> PointGrid -> S.Set Char
calcInfiniteChars Bounds {maxX = maxX, maxY = maxY} g =
    let edgePoints =
            [Point x y | y <- [0, maxY], x <- [0 .. maxX]] ++
            [Point x y | y <- [0 .. maxY], x <- [0, maxX]]
     in S.fromList $ L.map (\p -> C.toUpper $ g M.! p) edgePoints

calcNumPoints :: [Point] -> PointGrid -> S.Set Char -> M.Map Char Integer
calcNumPoints allPoints g infPoints = L.foldl' (accNumPoints g infPoints) M.empty allPoints

accNumPoints :: PointGrid -> S.Set Char -> M.Map Char Integer -> Point -> M.Map Char Integer
accNumPoints g infPoints acc p =
    let c = C.toUpper $ g M.! p
        inc = if S.member c infPoints then 0 else 1
    in M.insertWith (+) (C.toUpper $ g M.! p) inc acc

printMap :: [Point] -> Integer -> PointGrid -> IO ()
printMap allPoints r g = mapM_ printPoint allPoints
  where
    printPoint p@(Point {pX = pX, pY = pY}) =
        if pX == r
            then putStrLn $ [g M.! p]
            else putChar $ g M.! p

part1 :: [String] -> Integer
part1 input =
    let points = parseInput input
        pointChars = M.fromList $ zip points ['a' ..]
        bounds = calcBounds points
        allPoints = calcAllPoints bounds
        nearest = calcNearest allPoints pointChars
        infChars = calcInfiniteChars bounds nearest
        numPoints = calcNumPoints allPoints nearest infChars
    in last . L.sort . map snd . M.toList $ numPoints

runDay06Part1 :: IO String
runDay06Part1 = run "Day06-input.txt" part1
