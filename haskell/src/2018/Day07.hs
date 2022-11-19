module Day07 where

import Data.Map (Map)
import Text.Parsec (parse, oneOf)
import Text.Parsec.Char (char, string, letter, anyChar)
import Text.Parsec.String (Parser)

e =
    [ "Step C must be finished before step A can begin."
    , "Step C must be finished before step F can begin."
    , "Step A must be finished before step B can begin."
    , "Step A must be finished before step D can begin."
    , "Step B must be finished before step E can begin."
    , "Step D must be finished before step E can begin."
    , "Step F must be finished before step E can begin."
    ]

data Step
    = Step { id :: Char
           , prev :: Step
           , next :: Step }
    | Empty
    deriving (Show)

-- fst -> snd
type StepInput = (Char, Char)

parseInput :: [String] -> [StepInput]
parseInput input =
        case mapM (parse parseStep "") input of
            Right xs -> xs
            Left e -> error $ "Error parsing:" ++ show e

parseStep :: Parser StepInput
parseStep = (,)
    <$> (string "Step " *> anyChar)
    <* string " must be finished before step "
    <*> anyChar
    <* string " can begin."
