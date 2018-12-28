module Day04 where

import Text.Parsec.Char (string, digit, oneOf)
import Text.Parsec.String (Parser)
import Text.Parsec (many, (<|>), parse)
import Data.List (sort)

e =
    [ "[1518-11-01 00:00] Guard #10 begins shift"
    , "[1518-11-01 00:05] falls asleep"
    , "[1518-11-01 00:25] wakes up"
    , "[1518-11-01 00:30] falls asleep"
    , "[1518-11-01 00:55] wakes up"
    , "[1518-11-01 23:58] Guard #99 begins shift"
    , "[1518-11-02 00:40] falls asleep"
    , "[1518-11-02 00:50] wakes up"
    , "[1518-11-03 00:05] Guard #10 begins shift"
    , "[1518-11-03 00:24] falls asleep"
    , "[1518-11-03 00:29] wakes up"
    , "[1518-11-04 00:02] Guard #99 begins shift"
    , "[1518-11-04 00:36] falls asleep"
    , "[1518-11-04 00:46] wakes up"
    , "[1518-11-05 00:03] Guard #99 begins shift"
    , "[1518-11-05 00:45] falls asleep"
    , "[1518-11-05 00:55] wakes up"
    ]

eb = "[1518-11-01 00:00] Guard #10 begins shift"
es = "[1518-11-01 00:05] falls asleep"
ew = "[1518-11-01 00:25] wakes up"

data Log
    = Begin {id :: String}
    | Sleep {sDate :: String, sTime :: Int}
    | Wake {wDate :: String, wTime :: Int}
    deriving (Show)

guardText :: Parser Log
guardText = do
    string "Guard #"
    d <- many digit
    string " begins shift"
    return $ Begin d

sleepText :: String -> String -> Parser Log
sleepText d t = string "falls asleep" *> pure (Sleep d (read t))

wakeText :: String -> String -> Parser Log
wakeText d t = string "wakes up" *> pure (Wake d (read t))

parseLog :: Parser Log
parseLog = do
    string "[1518-"
    d <- many (oneOf "0123456789-")
    string " "
    many digit
    string ":"
    t <- many digit
    string "] "
    guardText <|> sleepText d t <|> wakeText d t

parseLogs :: [String] -> [Log]
parseLogs ls = case mapM (parse parseLog "") . sort $ ls of
    Right xs -> xs
    Left e -> error $ "Error parsing" ++ show e
