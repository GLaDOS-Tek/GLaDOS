module Structs (SExpr(..), Line) where

type Line = Int

data SExpr = Number Int Line
    | Literal String Line
    | Symbol String Line
    | List [SExpr]
    deriving (Show, Read, Eq)
