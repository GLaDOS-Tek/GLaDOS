module Main (main) where

-- IMPORTS

import Structs (SExpr(..), Ast(..))
import Parser (generalParser)
import Eval (sexprToAST, evalAst, Env)
import Tools (extractFromFile)
import Error (sendError, errorParams)
import System.Environment (getArgs)

-- FUNCTIONS

printOutput :: (Env, [Ast]) -> IO ()
printOutput (_, ast) = putStr "Glados-1.0 $ " >> print ast

main :: IO ()
main = getArgs >>= \args ->
    case args of
        [filepath] ->
            extractFromFile filepath >>= \content ->
                case generalParser 1 content of
                    Just (expr, _) ->
                        printOutput $ evalAst [] $ sexprToAST $ ExprList expr
                    _ -> sendError 84 "Error: Failed parsing"
        _ -> errorParams
