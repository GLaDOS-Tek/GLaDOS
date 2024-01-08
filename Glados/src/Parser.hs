module Parser (SExpr(..), parseChar, parseAnyChar, parseMany, parseSome, parseInt) where

-- IMPORTS

import Text.Read
import Structs (SExpr(..))
import Debug.Trace (trace)

-- CUSTOM TYPE

data Parser a = Parser {
    runParser :: String -> Maybe (a , String)
}

-- GLOBAL VAR

ascii_char :: [Char]
ascii_char = enumFromTo ' ' '~'

banned_char :: [Char]
banned_char = [' ', '(', ')', '\"']

valid_char :: [Char]
valid_char = filter (`notElem` banned_char) ascii_char

-- FUNCTIONS

getRestOfString :: String -> String
getRestOfString (' ':xs) = getRestOfString xs
getRestOfString str = drop 1 $ dropWhile (/= ' ') str

getFirstWord :: String -> String
getFirstWord "" = ""
getFirstWord (' ':xs) = getFirstWord xs
getFirstWord ('"':str) = case last str of
    '"' -> '"' : takeWhile (/= '"') str ++ "\""
    _ -> '"' : takeWhile (/= ' ') str
getFirstWord str = takeWhile (/= ' ') str

isLiteralEnd :: String -> Bool
isLiteralEnd "" = False
isLiteralEnd "\"" = True
isLiteralEnd ('\"':xs) = False
isLiteralEnd str = isLiteralEnd $ tail str

isLiteral :: String -> Bool
isLiteral "\"" = False
isLiteral ('\"':xs) = isLiteralEnd xs
isLiteral _ = False

literalToString :: String -> Maybe String
literalToString str = if head str == '"' && last str == '"'
    then Just $ tail $ init str
    else Nothing

parseOr :: Parser a -> Parser b -> Parser (Either a b)
parseOr p1 p2 = Parser $ \str ->
    case runParser p1 str of
        Just (a, b) -> Just (Left a, b)
        Nothing -> case runParser p2 str of
            Just (a, b) -> Just (Right a, b)
            Nothing -> Nothing

parseChar :: Char -> Parser Char
parseChar c = Parser $ \str ->
    case str of
        (x:xs) -> if x == c
            then Just (x, xs)
            else Nothing
        _ -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar list = Parser $ \str ->
    case (head str) `elem` list of
        True -> Just (head str, tail str)
        False -> Nothing

parseInt :: Parser Int
parseInt = Parser $ \str ->
    case (readMaybe (getFirstWord str) :: Maybe Int) of
        Just nbr -> Just (nbr, getRestOfString str)
        Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany parser = Parser $ \str ->
    case runParser parser str of
        Just (x, xs) -> case runParser (parseMany parser) xs of
            Just (y, ys) -> Just (x:y, ys)
            Nothing -> Just ([x], xs)
        Nothing -> Just ([], str)

parseSome :: Parser a -> Parser [a]
parseSome parser = Parser $ \str ->
    case runParser parser str of
        Just (x, xs) -> case runParser (parseMany parser) xs of
            Just (y, ys) -> Just ((x:y), ys)
            Nothing -> Just ([x], xs)
        Nothing -> Nothing

parseChain :: Parser a -> Parser [a]
parseChain parser = Parser $ \str ->
    case runParser parser str of
        Just (x, xs) -> case runParser (parseChain parser) xs of
            Just (y, ys) -> Just (x:y, ys)
            Nothing -> Just ([x], xs)
        Nothing -> Nothing

parseLiteral :: Parser String
parseLiteral = Parser $ \str ->
    case (isLiteral $ getFirstWord str) of
        True -> case (literalToString $ getFirstWord str) of
            Just lit -> Just (lit, getRestOfString str)
            _ -> Nothing
        _ -> Nothing

parseSymbol :: Parser String
parseSymbol = Parser $ \str -> Just (getFirstWord str, getRestOfString str)

