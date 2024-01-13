module Structs (SExpr(..), AstValue(..), Line) where

type Line = Int

data SExpr = SNumber Int Line
    | SLiteral String Line
    | SSymbol String Line
    | SList [SExpr]
    deriving (Show, Read, Eq)

data BinaryOp =
      Add AstValue AstValue
    | Sub AstValue AstValue
    | Mul AstValue AstValue
    | Div AstValue AstValue

data BoolOp =
      Lt AstValue AstValue
    | Lte AstValue AstValue
    | Gt AstValue AstValue
    | Gte AstValue AstValue
    | Eq AstValue AstValue

data AstValue =
      Number Int -- ok
    | Literal String -- ok
    | Boolean Bool -- ok
    | Symbol String -- ok
    | List [AstValue] -- ok/ko
    | Operator String AstValue AstValue
    | Define String AstValue
    | Cond AstValue AstValue AstValue
    | Call String [AstValue]
    | Func String AstValue [AstValue]
    | Error String Int
    deriving (Show, Eq)

-- instance Show Ast where
--     show (Number i) = show i
--     show (Str s) = "\"" ++ s ++ "\""
--     show (Symbol s) = s
--     show (Boolean b) = show b
--     show (Define str ast) = "Define " ++ str ++ " = " ++ show ast
--     show (Func name params body) = "(" ++ name ++  ")" ++ " => " ++ "(" ++ show params ++ "): " ++ "[" ++ show body ++ "]"
--     show (Call str asts) = "Call " ++ str ++ " (" ++ show asts ++ ")"
--     show (List asts) = "List " ++ show asts
--     show (Cond cond trueBranch falseBranch) = "If (" ++ show cond ++ ") then (" ++ show trueBranch ++ ") else (" ++ show falseBranch ++ ")"
--     show (Error errMsg line) = "Error (line " ++ show line ++ "): " ++ errMsg
