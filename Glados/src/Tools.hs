module Tools (
    extractFromFile,
    multiElem,
    replaceChar,
    first
) where

-- IMPORTS

import Error
import Control.Exception (catch)

-- FUNCTIONS

extractFromFile :: String -> IO String
extractFromFile filepath = readFile filepath `catch` exceptionMessage

multiElem :: Eq a => [a] -> [a] -> Bool
multiElem test list = any (`elem` list) test

replaceChar :: Char -> Char -> String -> String
replaceChar _ _ "" = ""
replaceChar c1 c2 (x:xs) = if x == c1
    then c2 : replaceChar c1 c2 xs
    else x : replaceChar c1 c2 xs

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)
