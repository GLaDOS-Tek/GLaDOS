module Parser (
  Parser,
  runParser,
  isWhitespace,
  parseWS,
  parseChar,
  parseAnyChar,
  parseMany,
  parseSome,
  parseSpan,
  parseUInt,
  parseInt,
  parseSymbol,
  parseString,
  parseWord,
  parseBoolean
) where

-- IMPORTS

import Control.Applicative
import Data.Char
import Text.Read (readMaybe)
import Tools

-- CUSTOM TYPE

data Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> fmap (first f) (p input)

instance Applicative Parser where
  pure v = Parser $ \input -> Just (v, input)
  (Parser p1) <*> (Parser p2) = Parser $ \input ->
    case p1 input of
      Just (f, rest1) -> case p2 rest1 of
        Just (v, rest2) -> Just (f v, rest2)
        Nothing -> Nothing
      Nothing -> Nothing

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \input ->
    case p input of
      Just (v, rest) -> runParser (f v) rest
      Nothing -> Nothing

-- FUNCTIONS

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` " \t\n"

parseWS :: Parser String
parseWS = Parser $ \input -> Just $ span isWhitespace input

parseChar :: Char -> Parser Char
parseChar c = Parser $ \input ->
    case input of
        (x:xs) -> if x == c
                    then Just (c, xs)
                    else Nothing
        _ -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar ref = Parser $ \input ->
    case input of
        (x:xs) -> (if x `elem` ref then Just (x, xs) else Nothing)
        _ -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \input ->
  case runParser p input of
    Just (x, xs) -> case runParser (parseMany p) xs of
      Just (y, ys) -> Just (x:y, ys)
      Nothing -> Just ([x], xs)
    Nothing -> Just ([], input)

parseSome :: Parser a -> Parser [a]
parseSome p = Parser $ \input ->
  case runParser p input of
    Just (x, xs) -> case runParser (parseSome p) xs of
        Just (y, ys) -> Just (x:y, ys)
        Nothing -> Just ([x], xs)
    Nothing -> Nothing

parseSpan :: (Char -> Bool) -> Parser String
parseSpan f = Parser $ \input -> Just (span f input)

parseUInt :: Parser Int
parseUInt = Parser $ \input ->
    case span isDigit input of
        ([], _) -> Nothing
        (out, rest) -> case readMaybe out :: Maybe Int of
            Just num -> Just (num, rest)
            _ -> Nothing

parseInt :: Parser Int
parseInt = Parser $ \input ->
    case runParser (parseChar '-') input of
        Just (_, xs) -> case runParser parseUInt xs of
            Just (num, ys) -> Just (-num, ys)
            _ -> Nothing
        _ -> runParser parseUInt input

parseSymbol :: Parser String
-- parseSymbol = parseSpan isAlpha
parseSymbol = parseSome $ parseAnyChar ['A'..'Z'] <|> parseAnyChar ['a'..'z']

parseString :: Parser String
parseString = parseChar '"' *> parseSpan (/= '"') <* parseChar '"'

parseWord :: String -> Parser String
parseWord input = traverse parseChar input

parseBoolean :: Parser Bool
parseBoolean = Parser $ \input -> case runParser parseSymbol input of
    Just ("true", xs)-> Just (True, xs)
    Just ("false", xs) -> Just (False, xs)
    _ -> Nothing
