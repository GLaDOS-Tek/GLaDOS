module Structs (SExpr(..)) where

type Line = Int

data Value = Number Int Line
    | Literal String Line
    | Symbol String Line
    | ValueList [Value]
    deriving (Show, Read, Eq)

data Parameters = Values [Value]
    deriving (Show, Read, Eq)

data Instruction = InstValue Value
    | InstParam Parameters
    deriving (Show, Read, Eq)

data Content = Instructions [Instruction]
    deriving (Show, Read, Eq)

data SExpr = Value Value
    | Parameters Parameters
    | Instruction Instruction
    | Content Content
    | ExprList [SExpr]
    deriving (Show, Read, Eq)

-- data Ast = Define String Ast
--     | Cond Ast Ast Ast
--     | Defun String [Ast] [Ast]
--     | Number Int
--     | Str String
--     | Symbol String
--     | Boolean Bool
--     | Call String [Ast]
--     | AstList [Ast]
--     | Error String Int
--     deriving (Read, Eq)

-- instance Show Ast where
--     show (Define str ast) = "Define " ++ str ++ " = " ++ show ast
--     show (Number i) = show i
--     show (Str s) = s
--     show (Symbol s) = s
--     show (Boolean b) = show b
--     show (Call str asts) = "Call " ++ str ++ " (" ++ show asts ++ ")"
--     show (AstList asts) = "AstList " ++ show asts
--     show (Error errMsg line) = "Error (line " ++ show line ++ "): " ++ errMsg
--     show (Cond cond trueBranch falseBranch) = "If (" ++ show cond ++ ") then (" ++ show trueBranch ++ ") else (" ++ show falseBranch ++ ")"
--     show (Defun funcName args body) = "Defun " ++ funcName ++ " " ++ show args ++ " " ++ show body
