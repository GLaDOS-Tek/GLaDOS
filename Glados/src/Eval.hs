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
              ("/", numericOp div),
              ("=", binaryOp unwrapNumber (==)),
              ("/=", binaryOp unwrapNumber (/=)),
              ("<", binaryOp unwrapNumber (<)),
              (">", binaryOp unwrapNumber (>)),
              (">=", binaryOp unwrapNumber (>=)),
              ("<=", binaryOp unwrapNumber (<=))]

apply :: String -> Ast -> Ast
apply op params = case lookup op primitives of
                    Just func -> func params
                    Nothing -> Error ("Function '" ++ op ++ "' is not a primitive.") 0

unwrapStr :: Ast -> String
unwrapStr (Str s) = s
unwrapStr _ = ""

unwrapNumber :: Ast -> Int
unwrapNumber (Number n) = n
unwrapNumber _ = 0

unwrapBool :: Ast -> Bool
unwrapBool (Boolean b) = b
unwrapBool _ = False

numericOp :: (Int -> Int -> Int) -> Ast -> Ast
numericOp op (AstList list) = Number (foldl1 op (map unwrapNumber list))
numericOp _ _ = Error "Numerical operation" 0

binaryOp :: (Ast -> a) -> (a -> a -> Bool) -> Ast -> Ast
binaryOp uw op (AstList [lhs, rhs]) = Boolean $ op (uw lhs) (uw rhs)
binaryOp _ _ _ = Error "Binary operation" 0

evalIf :: Ast -> Ast -> Ast -> Ast
evalIf condExpr trueBranch falseBranch =
    case evalAst condExpr of
        Boolean True -> evalAst trueBranch
        Boolean False -> evalAst falseBranch
        Number _ -> evalAst trueBranch
        _ -> Error "Condition in 'if' statement must evaluate to a boolean value" 0

evalAst :: Ast -> Ast
evalAst (Number num) = Number num
evalAst (Str str) = Str str
evalAst (Boolean bool) = Boolean bool
evalAst (Call func args) = apply func $ AstList (map evalAst args)
evalAst (Cond cond left right) = evalIf cond left right
evalAst (AstList list) = AstList (map evalAst list)
evalAst _ = Error "Evaluation" 0
