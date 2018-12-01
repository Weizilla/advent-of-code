module Lib
    ( run
    , display
    ) where

run :: (Show a) => FilePath -> ([String] -> a) -> IO String
run inputFile f = do
    contents <- readFile $ "inputs/" ++ inputFile
    return $ show . f . lines $ contents

display :: FilePath -> ([String] -> IO ()) -> IO ()
display inputFile f = do
    contents <- readFile $ "inputs/" ++ inputFile
    f . lines $ contents
