module Day06 where

import Text.Parsec.Char (string, digit)
import Text.Parsec.String (Parser)
import Text.Parsec (many, parse)

e = ["1, 1",
    "1, 6",
    "8, 3",
    "3, 4",
    "5, 5",
    "8, 9"]

data Point = Point Integer Integer deriving (Show)

parsePoint :: Parser Point
parsePoint = (\a b -> Point (read a) (read b)) <$> many digit <* string ", " <*> many digit

parseInput :: [String] -> [Point]
parseInput input = case mapM (parse parsePoint "") input of
    Right xs -> xs
    Left e -> error $ "Error parsing" ++ show e

