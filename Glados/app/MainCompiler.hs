module Main where

-- IMPORTS

import Ast
import Tools
import Error
import Compiler
import Bytecode
import System.Environment (getArgs)

-- FUNCTIONS

-- printOutput :: (Env, [Ast]) -> IO ()
-- printOutput (_, ast) = putStr "Glados-1.0 $ " >> print ast

main :: IO ()
main = getArgs >>= \args ->
    case args of
        [filepath] ->
            extractFromFile filepath >>= \content ->
                case sourceToAst content of
                    Just ast -> writeBinaryToFile "out.flno" (compileAst ast) >>
                                (putStrLn $ id "Glados 1.0.0 $> Source code successfully compiled.")
                    _ -> errorParams
        _ -> errorParams
