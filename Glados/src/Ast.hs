{-
-- EPITECH PROJECT, 2024
-- Glados
-- File description:
-- Ast
-}

module Ast (sexprToAST) where

import Structs

astDefine :: [SExpr] -> AstValue
astDefine (SSymbol sym line : val : rest) = case val of
            SLiteral v _ -> Define sym (Symbol v)
            SNumber v _ -> Define sym (Number v)
            SList l-> Define sym (sexprToAST (SList l))
astDefine _ = Error "too many arguments in define statement" (-1)

astFunction :: [SExpr] -> AstValue
astFunction [SSymbol name _, SList args, SList body] = Func name [sexprToAST (SList args)] [sexprToAST (SList body)]
astFunction _ = Error "Syntax error: Function definition." (-1)

astCondition :: [SExpr] -> AstValue
astCondition (cond : true : false : _) = Cond (sexprToAST cond) (sexprToAST true) (sexprToAST false)
astCondition _ = Error "Syntax Error: Conditional branching" (-1)

sexprToAST :: SExpr -> AstValue
sexprToAST (SNumber i _) = Number i
sexprToAST (SLiteral str _ ) = Literal str
sexprToAST (SSymbol s _) = Symbol s
sexprToAST (SList (SSymbol "feed" _ : xs)) = astDefine xs
sexprToAST (SList ((SSymbol "cat" _) : xs)) = astFunction xs
sexprToAST (SList (SSymbol "if" _ : xs)) = astCondition xs
-- sexprToAST (SList [lhs, SSymbol op _, rhs]) = astOperator [lhs, SSymbol op (-1), rhs]
-- sexprToAST (SList ((SSymbol calledFunc _) : args)) = Call calledFunc (map sexprToAST args)
sexprToAST (SList l) = List (map sexprToAST l)
