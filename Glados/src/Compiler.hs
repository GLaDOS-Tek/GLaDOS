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

-- FUNCTIONS

envIsBound :: Env -> String -> Bool
envIsBound env key = case envGet env key of
                        Just _ -> True
                        Nothing -> False

envGet :: Env -> String -> Maybe AstValue
envGet env key = lookup key env

envBind :: Env -> String -> AstValue -> Env
envBind env key val = if envIsBound env key then env else (key, val) : env

compileAstValue :: AstValue -> [Instruction]
compileAstValue (AstNumber n) = [Push (Numerical n)]
compileAstValue (AstBoolean b) = [Push (Boolean b)]
compileAstValue (AstSymbol s) = [PushEnv s]
compileAstValue (AstBinaryOp op left right) =
    compileAstValue left ++ compileAstValue right ++ [Push (Operator op), Call]
compileAstValue _ = [Push (Numerical 0)]

compileAst :: [AstValue] -> [Instruction]
compileAst input = concatMap compileAstValue input

-- numericOp :: (Int -> Int -> Int) -> AstValue -> AstValue
-- numericOp op (List list) = Number (foldl1 op (reverseList (map unwrapNumber list)))
-- numericOp _ _ = Error "Numerical operation" 0

-- binaryOp :: (AstValue -> a) -> (a -> a -> Bool) -> AstValue -> AstValue
-- binaryOp uw op (List [lhs, rhs]) = Boolean $ op (uw lhs) (uw rhs)
-- binaryOp _ _ _ = Error "Binary operation" 0
