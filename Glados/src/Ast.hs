module Ast (
    sourceToAst
) where

import Control.Applicative
import Parser
import Structs

parseNumberAst :: Parser AstValue
parseNumberAst = AstNumber <$> (parseWS *> parseInt <* parseWS)

parseSymbolAst :: Parser AstValue
parseSymbolAst = AstSymbol <$> (parseWS *> parseSymbol <* parseWS)

parseBooleanAst :: Parser AstValue
parseBooleanAst = AstBoolean <$> (parseWS *> parseBoolean <* parseWS)

parseOperator :: Parser Operator
parseOperator = parseWS *> parseOp <* parseWS
    where parseOp = Add <$ parseWord "+"
                <|> Sub <$ parseWord "-"
                <|> Mul <$ parseWord "*"
                <|> Div <$ parseWord "/"
                <|> Eq <$ parseWord "=="
                <|> Ne <$ parseWord "!="
                <|> Lt <$ parseWord "<"
                <|> Le <$ parseWord "<="
                <|> Gt <$ parseWord ">"
                <|> Ge <$ parseWord ">="
                <|> And <$ parseWord "&&"
                <|> Or <$ parseWord "||"

parseLiteralAst :: Parser AstValue
parseLiteralAst = AstLiteral <$> (parseWS *> parseString <* parseWS)

parseOperationAst :: Parser AstValue
parseOperationAst = do
    lhs <- parseWS *> parseBooleanAst <|> parseNumberAst <|> parseSymbolAst
    op <- parseWS *> parseOperator
    rhs <- parseWS *> parseAst
    return (AstBinaryOp op lhs rhs)

parseDefineAst :: Parser AstValue
parseDefineAst = do
    name <- parseWS *> parseWord "feed" *> parseSymbolAst
    value <- parseWS *> parseAst
    return (AstDefine name value)

parseFunctionAst :: Parser AstValue
parseFunctionAst = do
    name <- parseWS *> parseWord "cat" *> parseSymbolAst
    args <- parseWS *> parseArgListAst
    body <- parseWS *> parseBodyAst
    return (AstFunc name args body)

parseConditionAst :: Parser AstValue
parseConditionAst = do
    cond <- parseWS *> parseWord "if" *> parseArgListAst
    true <- parseWS *> parseBodyAst
    false <- parseWS *> parseWord "else" *> parseBodyAst
    return (AstCond cond true false)

parseCallAst :: Parser AstValue
parseCallAst = do
    name <- parseWS *> parseSymbolAst
    args <- parseWS *> parseArgListAst
    return (AstCall name args)

parseArgListAst :: Parser AstValue
parseArgListAst = AstArgList <$> (parseWS *> parseChar '(' *> parseMany parseAst <* parseChar ')' <* parseWS)

parseBodyAst :: Parser AstValue
parseBodyAst = AstBody <$> (parseWS *> parseChar '{' *> parseMany parseAst <* parseChar '}' <* parseWS)

parseListAst :: Parser AstValue
parseListAst = AstList <$> (parseWS *> parseChar '(' *> parseMany parseAst <* parseChar ')' <* parseWS)

parseAst :: Parser AstValue
parseAst = parseOperationAst
       <|> parseConditionAst
       <|> parseFunctionAst
       <|> parseCallAst
       <|> parseListAst
       <|> parseDefineAst
       <|> parseNumberAst
       <|> parseLiteralAst
       <|> parseBooleanAst
       <|> parseSymbolAst

sourceToAst :: String -> Maybe [AstValue]
sourceToAst input = case runParser (parseMany parseAst) input of
  Just (out, _) -> Just out
  _ -> Nothing
