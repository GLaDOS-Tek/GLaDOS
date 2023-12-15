module Main (main) where

import Parser (generalParser)
--import Eval
import Tools (extractFromFile)
import Error (sendError, errorParams)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \args ->
    case args of
        [filepath] -> extractFromFile filepath >>= \content ->
            case generalParser 1 content of
                Just (expr, _) -> putStrLn $ show expr
                _ -> sendError 84 "Error: Failed parsing"
        _ -> errorParams