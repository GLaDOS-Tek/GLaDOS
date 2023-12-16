module Main (main) where

-- IMPORTS

import Structs (SExpr(..))
import Parser (generalParser)
import Eval (sexprToAST, evalAst)
import Tools (extractFromFile)
import Error (sendError, errorParams)
import System.Environment (getArgs)

-- FUNCTIONS

main :: IO ()
main = getArgs >>= \args ->
    case args of
        [filepath] ->
            extractFromFile filepath >>= \content ->
                case generalParser 1 content of
                    Just (expr, _) ->
                        putStrLn $ show $ evalAst $ sexprToAST $ ExprList expr
                    _ -> sendError 84 "Error: Failed parsing"
        _ -> errorParams