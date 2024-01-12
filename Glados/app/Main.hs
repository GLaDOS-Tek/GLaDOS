module Main (main) where

-- IMPORTS

import Parser (generalParser)
import Ast (sexprToAST)
import Compiler (evalAst, Env)
import Tools (extractFromFile)
import Error (sendError, errorParams)
import System.Environment (getArgs)

-- FUNCTIONS

-- printOutput :: (Env, [Ast]) -> IO ()
-- printOutput (_, ast) = putStr "Glados-1.0 $ " >> print ast

main :: IO ()
main = getArgs >>= \args ->
    case args of
        [filepath] ->
            extractFromFile filepath >>= \content ->
                print $ show $ generalParser content
        _ -> errorParams
