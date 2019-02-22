module Day06 where

import Text.Parsec (many, parse)
import Text.Parsec.Char (digit, string)
import Text.Parsec.String (Parser)

e = ["1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"]

data Point = Point
    { pX :: Integer
    , pY :: Integer
    }

data Bounds = Bounds
    { minX :: Integer
    , minY :: Integer
    , maxX :: Integer
    , maxY :: Integer
    }

parsePoint :: Parser Point
parsePoint = (\a b -> Point (read a) (read b)) <$> many digit <* string ", " <*> many digit

parseInput :: [String] -> [Point]
parseInput input =
    case mapM (parse parsePoint "") input of
        Right xs -> xs
        Left e -> error $ "Error parsing" ++ show e

bounds :: [Point] -> Bounds
bounds ps =
    let maxX = maximum . map pX $ ps
        maxY = maximum . map pY $ ps
        minX = minimum . map pX $ ps
        minY = minimum . map pY $ ps
     in Bounds minX minY maxX maxY

inBounds :: Point -> Bounds -> Bool
inBounds p b = or [pX p == minX b, pY p == minY b, pX p == maxX b, pY p == maxY b]
