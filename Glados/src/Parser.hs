module Parser (SExpr(..)) where

-- IMPORTS

import Text.Read (readMaybe)
import Structs (SExpr(..), Line)
import Debug.Trace (trace)
import Control.Applicative (Alternative(..), empty, (<|>))

-- CUSTOM TYPE

data Token = Token String Line
    deriving (Show)

data Parser a = Parser {
    runParser :: String -> Maybe (a , String)
}
instance Functor Parser where
    fmap f (Parser p) = Parser $ \str ->
        case p str of
            Just (result, rest) -> Just (f result, rest)
            Nothing -> Nothing
instance Applicative Parser where
    pure v = Parser $ \str -> Just (v, str)
    (Parser p1) <*> (Parser p2) = Parser $ \str ->
        case p1 str of
            Just (f, rest1) -> case p2 rest1 of
                Just (result, rest2) -> Just (f result, rest2)
                Nothing -> Nothing
            Nothing -> Nothing
instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \str ->
        case p1 str of
            Nothing -> p2 str
            success -> success
instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \str ->
        case p str of
            Just (result, rest) -> runParser (f result) rest
            Nothing -> Nothing

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

cleanString :: Char -> String -> String
cleanString _ "" = ""
cleanString c (x:xs) = if x == c
    then cleanString c xs
    else x : cleanString c xs

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
