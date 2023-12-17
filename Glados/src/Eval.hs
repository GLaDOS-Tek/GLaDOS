module Eval (SExpr(..), Ast(..), sexprToAST, evalAst) where

-- IMPORTS

import Structs (SExpr(..), Ast(..))

-- FUNCTIONS

astKeyWordDefine :: [SExpr] -> Ast
astKeyWordDefine ([]) = Error "missing definition to define statement" (-1)
astKeyWordDefine (_:[]) = Error "missing symbol or value to define statement" (-1)
astKeyWordDefine (x:y:[]) = case (x, y) of
                        (SymbolExpr s _, StrExpr v _) -> Define s (Symbol v)
                        (SymbolExpr s _, IntExpr v _) -> Define s (Number v)
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
sexprToAST (SymbolExpr ('#':xs) _) = astKeyWordHashtag xs
sexprToAST (SymbolExpr s _) = Symbol s
sexprToAST (StrExpr s _) = Str s
sexprToAST (ExprList ((SymbolExpr "define" line):xs)) = case (output) of
                                                Error e (-1) -> Error e line
                                                Error _ _ -> output
                                                _ -> output
                                                where output = astKeyWordDefine xs
sexprToAST (ExprList [SymbolExpr "defun" line, SymbolExpr funcName _, ExprList args, body]) =
                                                case output of
                                                    Error e (-1) -> Error e line
                                                    Error _ _ -> output
                                                    _ -> output
                                                where output = Defun funcName (map sexprToAST args) [sexprToAST body]
sexprToAST (ExprList [SymbolExpr "if" _, condition, trueBranch, falseBranch]) = Cond (sexprToAST condition) (sexprToAST trueBranch) (sexprToAST falseBranch)
sexprToAST (ExprList ((SymbolExpr funcName _) : args)) = case args of
    args -> Call funcName (map sexprToAST args)
    _ -> Error "Invalid function application" (-1)
sexprToAST (ExprList l) = AstList (map sexprToAST l)

primitives :: [(String, Ast -> Ast)]
primitives = [("+", numericOp (+)),
              ("-", numericOp (-)),
              ("*", numericOp (*)),
              ("/", numericOp div)]

apply :: String -> Ast -> Ast
apply op params = case lookup op primitives of
                    Just func -> func params
                    Nothing -> Error ("Function '" ++ op ++ "' is not a primitive.") 0

extractNumber :: Ast -> Int
extractNumber (Number n) = n
extractNumber _ = 0

numericOp :: (Int -> Int -> Int) -> Ast -> Ast
numericOp op (AstList list) = Number (foldl1 op (map extractNumber list))
numericOp _ _ = Error "num" 0

evalAst :: Ast -> Ast
evalAst (Number num) = Number num
evalAst (Symbol sym) = Symbol sym
evalAst (Boolean bool) = Boolean bool
evalAst (Call op args) = apply op (AstList (map evalAst args))
evalAst (AstList list) = AstList (map evalAst list)
evalAst _ = Error "eval" 0
