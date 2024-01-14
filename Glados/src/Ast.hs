{-
-- EPITECH PROJECT, 2024
-- Glados
-- File description:
-- Ast
-}

module Ast (
    sourceToAst
) where

import Control.Applicative
import Parser
import Structs

parseNumberAst :: Parser AstValue
parseNumberAst = Number <$> (parseWS *> parseInt <* parseWS)

parseSymbolAst :: Parser AstValue
parseSymbolAst = Symbol <$> (parseWS *> parseSymbol <* parseWS)

parseBooleanAst :: Parser AstValue
parseBooleanAst = Boolean <$> (parseWS *> parseBoolean <* parseWS)

parseOperatorAst :: Parser AstValue
parseOperatorAst = Operator <$> (parseWS *> parseOp <* parseWS)
    where parseOp = Add <$ parseWord "+"
                <|> Sub <$ parseWord "-"
                <|> Mul <$ parseWord "*"
                <|> Div <$ parseWord "/"
                <|> Equal <$ parseWord "=="
                <|> NEqual <$ parseWord "!="

parseLiteralAst :: Parser AstValue
parseLiteralAst = Literal <$> (parseWS *> parseString <* parseWS)

parseDefineAst :: Parser AstValue
parseDefineAst = do
    name <- parseWS *> parseWord "feed" *> parseSymbolAst
    value <- parseWS *> parseAst
    return (Define name value)

parseFunctionAst :: Parser AstValue
parseFunctionAst = do
    name <- parseWS *> parseWord "cat" *> parseSymbolAst
    args <- parseWS *> parseArgListAst
    body <- parseWS *> parseBodyAst
    return (Func name args body)

parseConditionAst :: Parser AstValue
parseConditionAst = do
    cond <- parseWS *> parseWord "if" *> parseArgListAst
    true <- parseWS *> parseBodyAst
    false <- parseWS *> parseWord "else" *> parseBodyAst
    return (Cond cond true false)

parseCallAst :: Parser AstValue
parseCallAst = do
    name <- parseWS *> parseSymbolAst
    args <- parseWS *> parseArgListAst
    return (Call name args)

parseArgListAst :: Parser AstValue
parseArgListAst = ArgList <$> (parseWS *> parseChar '(' *> parseMany parseAst <* parseChar ')' <* parseWS)

parseBodyAst :: Parser AstValue
parseBodyAst = Body <$> (parseWS *> parseChar '{' *> parseMany parseAst <* parseChar '}' <* parseWS)

parseAst :: Parser AstValue
parseAst = parseBodyAst
       <|> parseArgListAst
       <|> parseDefineAst
       <|> parseConditionAst
       <|> parseFunctionAst
       <|> parseNumberAst
       <|> parseOperatorAst
       <|> parseCallAst
       <|> parseLiteralAst
       <|> parseBooleanAst
       <|> parseSymbolAst

sourceToAst :: String -> Maybe [AstValue]
sourceToAst input = case runParser (parseMany parseAst) input of
  Just (out, _) -> Just out
  _ -> Nothing
