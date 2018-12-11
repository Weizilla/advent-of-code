module Lib
    ( run
    ) where

run :: (Show a) => FilePath -> ([String] -> a) -> IO String
run inputFile f = do
    contents <- readFile $ "../inputs/" ++ inputFile
    return $ show . f . lines $ contents
