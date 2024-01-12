{-
-- EPITECH PROJECT, 2024
-- Glados
-- File description:
-- Compiler
-}

module Compiler (Env, evalAst) where

-- IMPORTS
import Structs (AstValue(..))

-- FUNCTIONS

type Env = [(String, AstValue)]

envIsBound :: Env -> String -> Bool
envIsBound env key = case envGet env key of
                        Just _ -> True
                        Nothing -> False

envGet :: Env -> String -> Maybe AstValue
envGet env key = lookup key env

envBind :: Env -> String -> AstValue -> Env
envBind env key val = case envIsBound env key of
                        True -> env
                        False -> (key, val) : env

primitives :: [(String, AstValue -> AstValue)]
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

-- unwrapStr :: AstValue -> String
-- unwrapStr (Str s) = s
-- unwrapStr _ = ""

unwrapNumber :: AstValue -> Int
unwrapNumber (Number n) = n
unwrapNumber _ = 0

unwrapBool :: AstValue -> Bool
unwrapBool (Boolean b) = b
unwrapBool _ = False

reverseList :: [a] -> [a]
reverseList list =  foldl (\acc x -> x : acc) [] list

numericOp :: (Int -> Int -> Int) -> AstValue -> AstValue
numericOp op (List list) = Number (foldl1 op (reverseList (map unwrapNumber list)))
numericOp _ _ = Error "Numerical operation" 0

binaryOp :: (AstValue -> a) -> (a -> a -> Bool) -> AstValue -> AstValue
binaryOp uw op (List [lhs, rhs]) = Boolean $ op (uw lhs) (uw rhs)
binaryOp _ _ _ = Error "Binary operation" 0

evalIf :: Env -> AstValue -> AstValue -> AstValue -> (Env, [AstValue])
evalIf env condExpr trueBranch falseBranch =
    case evalAst env condExpr of
        (env', [Boolean True]) -> evalAst env' trueBranch
        (env', [Boolean False]) -> evalAst env' falseBranch
        (env', [Number _]) -> evalAst env' trueBranch
        _ -> ([], [Error "Condition in 'if' statement must evaluate to a boolean value" 0])

apply :: Env -> String -> AstValue -> AstValue
apply env op params = case lookup op primitives of
                        Just func -> func params
                        Nothing -> case envGet env op of
                                    Just expr -> head (snd (evalAst env expr))
                                    _ -> Error ("Function '" ++ op ++ "' is not a primitive.") 0

evalAst :: Env -> AstValue -> (Env, [AstValue])
evalAst env (Number num) = (env, [Number num])
evalAst env (Literal str) = (env, [Literal str])
evalAst env (Boolean bool) = (env, [Boolean bool])
evalAst env (Symbol sym) = case envGet env sym of
                            Just val -> evalAst env val
                            Nothing -> (env, [Error "Variable is not bound" 0])
evalAst env (Define sym expr) = if envIsBound env sym
                                then (env, [Error "Variable is already bound" 0])
                                else (envBind env sym expr, [])
evalAst env (Cond cond left right) = evalIf env cond left right
evalAst env (Call func args) = case evalAst env (List args) of
                                (env', args') -> (env', [apply env func (List args')])
evalAst env (List list) =  foldl combinator (env, []) list
        where combinator :: (Env, [AstValue]) -> AstValue -> (Env, [AstValue])
              combinator (envcpy, ast) element = case evalAst envcpy element of
                        (env', elem') -> (env', elem' ++ ast)
evalAst _ _ = ([], [Error "Evaluation" 0])
