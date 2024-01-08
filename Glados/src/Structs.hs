module Structs (SExpr(..)) where

type Line = Int

type Symbol = String

data SExpr = Number Int Line
    | Literal String Line
    | Symbol Symbol Line
    | List [SExpr]
    deriving (Show, Read, Eq)
