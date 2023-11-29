module Eval (SExpr(..), Ast(..), sexprToAST) where

data SExpr = IntExpr Int Int -- second int is the line
    | StrExpr String Int
    | ExprList [SExpr]
    deriving (Show, Read)

data Ast = Define String Ast
    | Number Int
    | Symbol String
    | Boolean Bool
    | Call String [Ast]
    | AstList [Ast]
    | Error String Int
    deriving (Read)

instance Show Ast where
    show (Define str ast) = "Define " ++ str ++ " = " ++ show ast
    show (Number i) = show i
    show (Symbol s) = s
    show (Boolean b) = show b
    show (Call str asts) = "Call " ++ str ++ "(" ++ show asts ++ ")"
    show (AstList asts) = "AstList [" ++ show asts ++ "]"
    show (Error errMsg line) = "Error (line " ++ show line ++ "): " ++ errMsg

astKeyWordDefine :: [SExpr] -> Ast
astKeyWordDefine ([]) = Error "missing definition to define statement" (-1)
astKeyWordDefine (_:[]) = Error "missing symbol or value to define statement" (-1)
astKeyWordDefine (x:y:[]) = case (x, y) of
                        (StrExpr s _, StrExpr v _) -> Define s (Symbol v)
                        (StrExpr s _, IntExpr v _) -> Define s (Number v)
                        (StrExpr s line, ExprList l) -> case (sexprToAST (ExprList l)) of
                            Error e _ -> Error ("invalid value for define statement -> " ++ e) line
                            ast -> Define s ast
                        _ -> Error "invalid symbol or value to define statement" (-1)
astKeyWordDefine _ = Error "too many arguments to define statement" (-1)

astKeyWordHashtag :: String -> Ast
astKeyWordHashtag ("t") = Boolean True
astKeyWordHashtag ("f") = Boolean False
astKeyWordHashtag s = Symbol s

sexprToAST :: SExpr -> Ast
sexprToAST (IntExpr i _) = Number i
sexprToAST (StrExpr ('#':xs) _) = astKeyWordHashtag xs
sexprToAST (StrExpr s _) = Symbol s
sexprToAST (ExprList ((StrExpr "define" line):xs)) = case (output) of
                                                Error e (-1) -> Error e line
                                                Error _ _ -> output
                                                _ -> output
                                                where output = astKeyWordDefine xs
sexprToAST (ExprList l) = AstList (map sexprToAST l)
