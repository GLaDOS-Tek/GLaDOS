module Eval (Env, sexprToAST, evalAst) where

-- IMPORTS

import Structs (SExpr(..), Ast(..))

-- FUNCTIONS

astKeyWordDefine :: [SExpr] -> Ast
astKeyWordDefine ([]) = Error "missing definition to define statement" (-1)
astKeyWordDefine (_:[]) = Error "missing symbol or value to define statement" (-1)
astKeyWordDefine (x:y:[]) = case (x, y) of
                        (SymbolExpr s _, StrExpr v _) -> Define s (Symbol v)
                        (SymbolExpr s _, IntExpr v _) -> Define s (Number v)
                        (SymbolExpr s line, ExprList l) -> case sexprToAST (ExprList l) of
                            Error e _ -> Error ("invalid value for define statement -> " ++ e) line
                            ast -> Define s ast
                        _ -> Error "invalid symbol or value to define statement" (-1)
astKeyWordDefine _ = Error "too many arguments to define statement" (-1)

astKeyWordHashtag :: String -> Ast
astKeyWordHashtag "t" = Boolean True
astKeyWordHashtag "f" = Boolean False
astKeyWordHashtag s = Symbol s

sexprToAST :: SExpr -> Ast
sexprToAST (IntExpr i _) = Number i
sexprToAST (StrExpr str _ ) = Str str
sexprToAST (SymbolExpr ('#':xs) _) = astKeyWordHashtag xs
sexprToAST (SymbolExpr s _) = Symbol s
sexprToAST (ExprList ((SymbolExpr "define" line):xs)) = case output of
                                                Error e (-1) -> Error e line
                                                Error _ _ -> output
                                                _ -> output
                                                where output = astKeyWordDefine xs
sexprToAST (ExprList [SymbolExpr "defun" _, SymbolExpr funcName _, ExprList args, body]) = output
                                                where output = Defun funcName (map sexprToAST args) [sexprToAST body]
sexprToAST (ExprList [SymbolExpr "if" _, condition, trueBranch, falseBranch]) = Cond (sexprToAST condition) (sexprToAST trueBranch) (sexprToAST falseBranch)
sexprToAST (ExprList ((SymbolExpr funcName _) : args)) = Call funcName (map sexprToAST args)
sexprToAST (ExprList l) = AstList (map sexprToAST l)

type Env = [(String, Ast)]

envIsBound :: Env -> String -> Bool
envIsBound env key = case envGet env key of
                        Just _ -> True
                        Nothing -> False

envGet :: Env -> String -> Maybe Ast
envGet env key = lookup key env

envBind :: Env -> String -> Ast -> Env
envBind env key val = case envIsBound env key of
                        True -> env
                        False -> (key, val) : env

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
              ("<=", binaryOp unwrapNumber (<=)),
              ("=", binaryOp unwrapBool (==)),
              ("/=", binaryOp unwrapBool (/=))]

-- unwrapStr :: Ast -> String
-- unwrapStr (Str s) = s
-- unwrapStr _ = ""

unwrapNumber :: Ast -> Int
unwrapNumber (Number n) = n
unwrapNumber _ = 0

unwrapBool :: Ast -> Bool
unwrapBool (Boolean b) = b
unwrapBool _ = False

reverseList :: [a] -> [a]
reverseList list =  foldl (\acc x -> x : acc) [] list

numericOp :: (Int -> Int -> Int) -> Ast -> Ast
numericOp op (AstList list) = Number (foldl1 op (reverseList (map unwrapNumber list)))
numericOp _ _ = Error "Numerical operation" 0

binaryOp :: (Ast -> a) -> (a -> a -> Bool) -> Ast -> Ast
binaryOp uw op (AstList [lhs, rhs]) = Boolean $ op (uw lhs) (uw rhs)
binaryOp _ _ _ = Error "Binary operation" 0

evalIf :: Env -> Ast -> Ast -> Ast -> (Env, [Ast])
evalIf env condExpr trueBranch falseBranch =
    case evalAst env condExpr of
        (env', [Boolean True]) -> evalAst env' trueBranch
        (env', [Boolean False]) -> evalAst env' falseBranch
        (env', [Number _]) -> evalAst env' trueBranch
        _ -> ([], [Error "Condition in 'if' statement must evaluate to a boolean value" 0])

apply :: Env -> String -> Ast -> Ast
apply env op params = case lookup op primitives of
                        Just func -> func params
                        Nothing -> case envGet env op of
                                    Just expr -> head (snd (evalAst env expr))
                                    _ -> Error ("Function '" ++ op ++ "' is not a primitive.") 0

evalAst :: Env -> Ast -> (Env, [Ast])
evalAst env (Number num) = (env, [Number num])
evalAst env (Str str) = (env, [Str str])
evalAst env (Boolean bool) = (env, [Boolean bool])
evalAst env (Symbol sym) = case envGet env sym of
                            Just val -> evalAst env val
                            Nothing -> (env, [Error "Variable is not bound" 0])
evalAst env (Define sym expr) = if envIsBound env sym
                                then (env, [Error "Variable is already bound" 0])
                                else (envBind env sym expr, [])
evalAst env (Cond cond left right) = evalIf env cond left right
evalAst env (Call func args) = case evalAst env (AstList args) of
                                (env', args') -> (env', [apply env func (AstList args')])
evalAst env (AstList list) =  foldl combinator (env, []) list
        where combinator :: (Env, [Ast]) -> Ast -> (Env, [Ast])
              combinator (envcpy, ast) element = case evalAst envcpy element of
                        (env', elem') -> (env', elem' ++ ast)
evalAst _ _ = ([], [Error "Evaluation" 0])
