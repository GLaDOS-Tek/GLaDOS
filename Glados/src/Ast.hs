{-
-- EPITECH PROJECT, 2024
-- Glados
-- File description:
-- Ast
-}

module Ast (sexprToAST) where

import Structs

type Env = [(String, AstValue)]

astDefine :: [SExpr] -> AstValue
astDefine (SSymbol sym line : val : rest) = case val of
           SLiteral v _ -> Define sym (Symbol v)
           SNumber v _ -> Define sym (Number v)
           SList l-> Define sym (sexprToAST (SList l))
astDefine _ = Error "too many arguments in define statement" (-1)

astFunction :: [SExpr] -> AstValue
astFunction [SSymbol name _, SList args, SList body] = Func name (sexprToAST $ SList args) [sexprToAST (SList body)]
astFunction _ = Error "Syntax error: Function definition." (-1)

astCondition :: [SExpr] -> AstValue
astCondition [cond, true, SSymbol "else" _, false] = Cond (sexprToAST cond) (sexprToAST true) (sexprToAST false)
astCondition _ = Error "Syntax Error: Conditional branching" (-1)

astCall :: String -> [SExpr] -> AstValue
astCall func args = Call func (map sexprToAST args)

sexprToAST :: SExpr -> AstValue
sexprToAST (SNumber i _) = Number i
sexprToAST (SLiteral str _) = Literal str
sexprToAST (SSymbol s _) = Symbol s
sexprToAST (SList (SSymbol "feed" _ : xs)) = astDefine xs
sexprToAST (SList ((SSymbol "cat" _) : xs)) = astFunction xs
sexprToAST (SList (SSymbol "if" _ : xs)) = astCondition xs
sexprToAST (SList [lhs, SSymbol "+" _, rhs]) = Operator "+" (sexprToAST lhs) (sexprToAST rhs)
sexprToAST (SList [SSymbol funcName _, SList args]) = astCall funcName args
sexprToAST (SList l) = List (map sexprToAST l)
