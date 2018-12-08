module Lib
    ( run
    ) where

run :: FilePath -> (String -> Integer) -> IO Integer
run inputFile f = do
    contents <- readFile inputFile
    return $ f contents
