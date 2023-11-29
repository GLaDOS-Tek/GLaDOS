module Main (main) where

import Eval

main :: IO ()
main = putStrLn $ show $ sexprToAST $ ExprList [StrExpr "define" 1, StrExpr "bob" 1, IntExpr 6 1]
