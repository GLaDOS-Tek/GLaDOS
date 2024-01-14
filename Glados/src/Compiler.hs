{-
-- EPITECH PROJECT, 2024
-- Glados
-- File description:
-- Compiler
-}

module Compiler (
    compileAst
) where

-- IMPORTS
import Structs
import Data.Maybe (fromJust)
import Data.List (elemIndex)

-- FUNCTIONS

envIsBound :: EnvVM -> String -> Bool
envIsBound env key = case envGet env key of
                        Just _ -> True
                        Nothing -> False

envGet :: EnvVM -> String -> Maybe Value
envGet env key = lookup key env

envBind :: EnvVM -> String -> Value -> EnvVM
envBind env key val = if envIsBound env key then env else (key, val) : env

compileCondition :: AstValue -> [Instruction]
-- compileCondition (AstCond c t f) = do
--     condInsts <- compileConditon
--     trueInsts <- compileTrueBlock
--     falseInsts <- compileFalseBlock
--     condInsts ++ [JumpIfFalse (length trueInsts)] ++ falseInsts
compileCondition _ = []

compileBody :: AstValue -> [Instruction]
compileBody (AstBody body) =
    case foldl compileAstValue ([], []) body of
        (_, insts) -> insts

compileFunction :: (EnvVM, [Instruction]) -> AstValue -> [Instruction]
compileFunction (e, i) (AstFunc (AstSymbol sym) (AstArgList args) body) =
    let rawArgs = map (\(AstSymbol arg) -> arg) (reverse args)
        bodyInsts = replaceFuncArgs rawArgs $ compileBody body
    in bodyInsts ++ [Ret]

replaceFuncArgs :: [String] -> [Instruction] -> [Instruction]
replaceFuncArgs args = map replacePushEnv
    where
        replacePushEnv (PushEnv v) = if v `elem` args
                                     then PushArg (fromJust $ elemIndex v args)
                                     else PushEnv v
        replacePushEnv i = i

compileFuncCall :: AstValue -> [Instruction]
compileFuncCall (AstCall (AstSymbol name) (AstArgList args)) =
    let insts = concatMap (snd . compileAstValue ([], [])) (reverse args)
    in insts ++ [PushEnv name] ++ [Call]

compileAstValue :: (EnvVM, [Instruction]) -> AstValue -> (EnvVM, [Instruction])
compileAstValue (e, i) (AstNumber n) = (e, [Push (Numerical n)])
compileAstValue (e, i) (AstBoolean b) = (e, [Push (Boolean b)])
compileAstValue (e, i) (AstSymbol s) = (e, [PushEnv s])
compileAstValue (e, i) (AstBinaryOp op left right) =
    let (_, cLeft) = compileAstValue (e, i) left
        (_, cRight) = compileAstValue (e, i) right
    in (e, cLeft ++ cRight ++ [Push (Operator op), Call])
compileAstValue (e, i) (AstDefine (AstSymbol sym) val) =
    case val of
        (AstBoolean b) -> (envBind e sym (Boolean b), i)
        (AstNumber n) -> (envBind e sym (Numerical n), i)
        _ -> (e, i)
compileAstValue (e, i) (AstFunc (AstSymbol sym) (AstArgList args) (AstBody body)) =
    let funcInsts = compileFunction (e, i) (AstFunc (AstSymbol sym) (AstArgList args) (AstBody body))
        e' = envBind e sym (Function funcInsts)
    in (e', i)
compileAstValue (e, i) (AstCall (AstSymbol func) (AstArgList args)) = (e, compileFuncCall (AstCall (AstSymbol func) (AstArgList args)))
compileAstValue (e, i) _ = (e, i)
-- compileAstValue val@(AstCond c t f) = compileCondition val

compileAst :: [AstValue] -> (EnvVM, [Instruction])
compileAst = foldl compileAstValue ([], [])

-- numericOp :: (Int -> Int -> Int) -> AstValue -> AstValue
-- numericOp op (List list) = Number (foldl1 op (reverseList (map unwrapNumber list)))
-- numericOp _ _ = Error "Numerical operation" 0

-- binaryOp :: (AstValue -> a) -> (a -> a -> Bool) -> AstValue -> AstValue
-- binaryOp uw op (List [lhs, rhs]) = Boolean $ op (uw lhs) (uw rhs)
-- binaryOp _ _ _ = Error "Binary operation" 0
