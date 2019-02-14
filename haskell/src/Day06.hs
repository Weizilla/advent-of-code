module Day06 where

import Text.Parsec.Char (string, digit)
import Text.Parsec.String (Parser)
import Text.Parsec (many)

e = ["1, 1",
    "1, 6",
    "8, 3",
    "3, 4",
    "5, 5",
    "8, 9"]

type Point = (Integer, Integer)

--parse :: [String] -> [Point]

pointText :: Parser Point
pointText = do
    x <- many digit
    string ", "
    y <- many digit
    return (read x, read y)
