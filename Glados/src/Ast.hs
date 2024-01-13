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

operators :: [(String, AstValue)]
operators = [("+", Operator "+"),
              ("-", Operator "-"),
              ("*", Operator "*"),
              ("/", Operator "/")]
            --   ("=", binaryOp unwrapNumber (==)),
            --   ("/=", binaryOp unwrapNumber (/=)),
            --   ("<", binaryOp unwrapNumber (<)),
            --   (">", binaryOp unwrapNumber (>)),
            --   (">=", binaryOp unwrapNumber (>=)),
            --   ("<=", binaryOp unwrapNumber (<=)),
            --   ("=", binaryOp unwrapBool (==)),
            --   ("/=", binaryOp unwrapBool (/=))]

parseNumberAst :: Parser AstValue
parseNumberAst = Number <$> (parseWS *> parseInt <* parseWS)

parseSymbolAst :: Parser AstValue
parseSymbolAst = Symbol <$> (parseWS *> parseSymbol <* parseWS)

parseBooleanAst :: Parser AstValue
parseBooleanAst = Boolean <$> (parseWS *> parseBoolean <* parseWS)

parseOperatorAst :: Parser AstValue
parseOperatorAst = Operator <$> (parseWS *> parseWord "+" <|> parseWord "-" <|> parseWord "*" <|> parseWord "/" <* parseWS)
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

parseArgListAst :: Parser AstValue
parseArgListAst = ArgList <$> (parseWS *> parseChar '(' *> parseMany parseAst <* parseChar ')' <* parseWS)

parseBodyAst :: Parser AstValue
parseBodyAst = Body <$> (parseWS *> parseChar '{' *> parseMany parseAst <* parseChar '}' <* parseWS)

parseAst :: Parser AstValue
parseAst = parseBodyAst
       <|> parseArgListAst
       <|> parseOperatorAst
       <|> parseDefineAst
       <|> parseConditionAst
       <|> parseFunctionAst
       <|> parseNumberAst
       <|> parseLiteralAst
       <|> parseBooleanAst
       <|> parseSymbolAst

sourceToAst :: String -> Maybe [AstValue]
sourceToAst input = case runParser (parseMany parseAst) input of
  Just (out, _) -> Just out
  _ -> Nothing
