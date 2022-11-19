module Day04 where

import Text.Parsec.Char (string, digit, oneOf)
import Text.Parsec.String (Parser)
import Text.Parsec (many, (<|>), parse)
import Data.List.Split (chunk)
import Data.List (sort, sortOn, group, maximumBy)
import Data.Map.Strict (Map, insertWith, toList, (!), singleton, unionWith)
import Data.Function (on)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Lib (run)

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

st = groupByDate [] M.empty . parseLogs $ e
at = allTimes $ st
-- ts = timeSleeps . allTimes $ st

type GuardId = Integer
type Date = String
type Time = Integer

data Log
    = Begin {bId :: GuardId}
    | Sleep {sDate :: Date, sTime :: Time}
    | Wake {wDate :: Date, wTime :: Time}
    deriving (Show)


guardText :: Parser Log
guardText = Begin . read <$> (string "Guard #" *> many digit <* string " begins shift")

sleepText :: Date -> String -> Parser Log
sleepText d t = Sleep d (read t) <$ string "falls asleep"

wakeText :: Date -> String -> Parser Log
wakeText d t = Wake d (read t) <$ string "wakes up"

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

type SleepTimes = Map GuardId [Time]

groupByDate :: [Log] -> SleepTimes -> [Log] -> SleepTimes
groupByDate curr total [] = accumulate curr total
groupByDate curr total (x : xs) = case x of
    Begin { bId = i } -> groupByDate [x] t xs
        where t = if null curr then total else accumulate curr total
    _ -> groupByDate (x : curr) total xs

accumulate :: [Log] -> SleepTimes -> SleepTimes
accumulate curr total =
    let
        rCurr = reverse curr
        Begin { bId = guardId } = head rCurr
        st = calcSleepTimes [] . tail $ rCurr
        m = insertWith (++) guardId st total
    in m

calcSleepTimes :: [Time] -> [Log] -> [Time]
calcSleepTimes total [] = total
calcSleepTimes total (Sleep { sTime = t1 } : Wake { wTime = t2 } : ls) =
    calcSleepTimes ([t1 .. (t2 - 1)] ++ total) ls

mostSleptGuard :: SleepTimes -> GuardId
mostSleptGuard = fst . last . sortOn snd . toList . M.map length

mostSleptTime :: GuardId -> SleepTimes -> Time
mostSleptTime guardId st =
    head . snd . maximum . map (\x -> (length x, x)) . group . sort $ st ! guardId

part1 :: [String] -> Integer
part1 input =
    let
        st = groupByDate [] M.empty . parseLogs $ input
        gId = mostSleptGuard st
        t = mostSleptTime gId st
    in gId * t

runDay04Part1 :: IO String
runDay04Part1 = run "Day04-input.txt" part1

type TimeSleeps = Map Time (Map GuardId Integer)

allTimes :: SleepTimes -> [(Time, GuardId)]
allTimes = concatMap (\(g, xs) -> zip xs (repeat g)) . toList

maxSleep :: [(Time, GuardId)] -> (Time, GuardId)
maxSleep = head . snd . maximumBy (compare `on` fst) . map (\l -> (length l, l)) . group . sort

part2 :: [String] -> Integer
part2 input =
    let
        (t, g) = maxSleep . allTimes $ st
        st = groupByDate [] M.empty . parseLogs $ input
    in t * g

runDay04Part2 :: IO String
runDay04Part2 = run "Day04-input.txt" part2