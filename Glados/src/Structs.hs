module Structs (
  Env,
  BinOp(..),
  AstValue(..)
) where

type Env = [(String, AstValue)]

data BinOp =
      Add
    | Sub
    | Div
    | Mul
    | Greater
    | Lower
    | Equal
    | NEqual
    deriving (Show, Eq)

data AstValue =
      Number Int
    | Literal String
    | Boolean Bool
    | Symbol String
    | List [AstValue]
    | ArgList [AstValue]
    | Body [AstValue]
    | Define AstValue AstValue
    | Cond AstValue AstValue AstValue
    | Call AstValue AstValue
    | Func AstValue AstValue AstValue
    | Operator BinOp
    | Error String Int
    deriving (Show, Eq)
