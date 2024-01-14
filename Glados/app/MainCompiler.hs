module Main where

-- IMPORTS

import Ast
import Tools
import Error
import System.Environment (getArgs)

-- FUNCTIONS

-- printOutput :: (Env, [Ast]) -> IO ()
-- printOutput (_, ast) = putStr "Glados-1.0 $ " >> print ast

main :: IO ()
main = getArgs >>= \args ->
    case args of
        [filepath] ->
            extractFromFile filepath >>= \content ->
                putStrLn $ show $ sourceToAst content
        _ -> errorParams
