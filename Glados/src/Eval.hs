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
sexprToAST (ExprList (funcExpr : args)) = case funcExpr of
                                        (StrExpr funcName _) -> Call funcName (map sexprToAST args)
                                        _ -> Error "Invalid function application" (-1)
sexprToAST (ExprList l) = AstList (map sexprToAST l)

sumAst :: Maybe Ast -> Maybe Ast -> Maybe Ast
sumAst (Just (Number lhs)) (Just (Number rhs)) = Just $ Number $ lhs + rhs
sumAst _ _ = Nothing

mulAst :: Maybe Ast -> Maybe Ast -> Maybe Ast
mulAst (Just (Number lhs)) (Just (Number rhs)) = Just $ Number $ lhs * rhs
mulAst _ _ = Nothing

subAst :: Maybe Ast -> Maybe Ast -> Maybe Ast
subAst (Just (Number lhs)) (Just (Number rhs)) = Just $ Number $ lhs - rhs
subAst _ _ = Nothing

evalAst :: Ast -> Maybe Ast
evalAst (Number num) = Just (Number num)
evalAst (Symbol sym) = Just (Symbol sym)
evalAst (Boolean bool) = Just (Boolean bool)
evalAst (Call "+" (x:y:_)) = sumAst (evalAst x) (evalAst y)
evalAst (Call "*" (x:y:_)) = mulAst (evalAst x) (evalAst y)
evalAst (Call "-" (x:y:_)) = subAst (evalAst x) (evalAst y)
evalAst _ = Nothing