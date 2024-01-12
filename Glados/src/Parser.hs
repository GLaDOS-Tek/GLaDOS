module Parser (SExpr(..)) where

-- IMPORTS

import Text.Read (readMaybe)
import Structs (SExpr(..), Line)
import Control.Applicative (Alternative(..), empty, (<|>))
import Tools (multiElem, replaceChar, first)

-- CUSTOM TYPE

data Token = Token String Line
    deriving (Show)

data Parser a = Parser {
    runParser :: [Token] -> Maybe (a, [Token])
}
instance Functor Parser where
    fmap f (Parser p) = Parser $ \token -> fmap (first f) (p token)
instance Applicative Parser where
    pure v = Parser $ \_ -> Just (v, [])
    (Parser p1) <*> (Parser p2) = Parser $ \token ->
        case p1 token of
            Just (f, rest1) -> case p2 rest1 of
                Just (v, rest2) -> Just (f v, rest2)
                Nothing -> Nothing
            Nothing -> Nothing
instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \token -> p1 token <|> p2 token
instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \token ->
        case p token of
            Just (v, rest) -> runParser (f v) rest
            Nothing -> Nothing

-- GLOBAL VAR

banned_char :: [Char]
banned_char = [' ', '(', ')', '\"', '\'', '`', ';', '{', '}', '[', ']', '\n', '\t', '\r']

-- FUNCTIONS

getRestOfString :: String -> String
getRestOfString (' ':xs) = getRestOfString xs
getRestOfString ('\n':xs) = getRestOfString xs
getRestOfString ('\"':xs) = dropWhile (\c -> c /= ' ' && c /= '\n') (dropWhile (/= '\"') xs)
getRestOfString str = dropWhile (\c -> c /= ' ' && c /= '\n') str

getFirstToken :: String -> String
getFirstToken "" = ""
getFirstToken (' ':xs) = getFirstToken xs
getFirstToken ('\n':xs) = getFirstToken xs
getFirstToken ('\"':xs) = '\"' : (takeWhile (/= '\"') xs) ++ takeWhile (\c -> c /= ' ' && c /= '\n') (dropWhile (/= '\"') xs)
getFirstToken str = takeWhile (\c -> c /= ' ' && c /= '\n') str

getLiteral :: String -> String
getLiteral "" = ""
getLiteral ('\"':xs) = '\"' : (takeWhile (/= '\"') xs) ++ "\""
getLiteral str = str

skipLiteral :: String -> String
skipLiteral "" = ""
skipLiteral ('\"':xs) = tail $ dropWhile (/= '\"') xs
skipLiteral str = str

spacesAroundLists :: String -> String
spacesAroundLists "" = ""
spacesAroundLists ('\"':xs) = getLiteral ('\"':xs) ++ (spacesAroundLists $ skipLiteral ('\"':xs))
spacesAroundLists ('(':xs) = " ( " ++ spacesAroundLists xs
spacesAroundLists (')':xs) = " ) " ++ spacesAroundLists xs
spacesAroundLists ('[':xs) = " [ " ++ spacesAroundLists xs
spacesAroundLists (']':xs) = " ] " ++ spacesAroundLists xs
spacesAroundLists ('{':xs) = " { " ++ spacesAroundLists xs
spacesAroundLists ('}':xs) = " } " ++ spacesAroundLists xs
spacesAroundLists (';':xs) = " ; " ++ spacesAroundLists xs
spacesAroundLists (x:xs) = x : spacesAroundLists xs

customWords :: Line -> String -> [Token]
customWords _ "" = []
customWords line (' ':xs) = customWords line xs
customWords line ('\n':xs) = customWords (line + 1) xs
customWords line str = Token (getFirstToken str) line : customWords line (getRestOfString str)

stringToTokens :: String -> [Token]
stringToTokens str = customWords 1 (spacesAroundLists (replaceChar '\t' ' ' str))

isSymbol :: Token -> Bool
isSymbol (Token "" _) = False
isSymbol (Token str l) = not $ multiElem str banned_char

isLiteral :: String -> Bool
isLiteral "" = False
isLiteral "\"" = False
isLiteral str = head str == '"' && last str == '"'

-- if success, return the line number of the token
isChar :: Char -> Parser Int
isChar c = Parser $ \((Token x l):rest) ->
    if x == [c]
        then Just (l, rest)
        else Nothing

parseChar :: Char -> Parser Char
parseChar c = Parser $ \((Token x l):rest) ->
    if x == [c]
        then Just (c, rest)
        else Nothing

runParserSafe :: Parser a -> [Token] -> Maybe (a, [Token])
runParserSafe p [] = Nothing
runParserSafe p tokens = runParser p tokens

parseNumber :: Parser SExpr
parseNumber = Parser $ \((Token str l):rest) ->
    case (readMaybe (str) :: Maybe Int) of
        Just nbr -> Just (Number nbr l, rest)
        Nothing -> Nothing

parseSymbol :: Parser SExpr
parseSymbol = Parser $ \((Token str l):rest) ->
    if isSymbol (Token str l)
        then Just (Symbol str l, rest)
        else Nothing

parseLiteral :: Parser SExpr
parseLiteral = Parser $ \((Token str l):rest) ->
    if isLiteral str
        then Just (Literal (tail $ init str) l, rest)
        else Nothing

parseMany :: Parser a -> Parser [a]
parseMany parser = Parser $ \tokens ->
    case runParserSafe (parseChar ';') tokens of
        Just (_, rest) -> Just ([], rest)
        Nothing -> case runParserSafe parser tokens of
            Just (x, xs) -> case runParser (parseMany parser) xs of
                Just (y, ys) -> Just (x:y, ys)
                Nothing -> Just ([x], xs)
            Nothing -> Just ([], tokens)

parseSome :: Parser a -> Parser [a]
parseSome parser = Parser $ \str ->
    case runParserSafe parser str of
        Just (x, xs) -> case runParser (parseMany parser) xs of
            Just (y, ys) -> Just ((x:y), ys)
            Nothing -> Just ([x], xs)
        Nothing -> Nothing

parseSExpr :: Parser SExpr
parseSExpr = parseNumber <|> parseSymbol <|> parseLiteral <|> parseList '(' ')' <|> parseList '[' ']' <|> parseList '{' '}'

parseList :: Char -> Char -> Parser SExpr
parseList open close = Parser $ \(tokens) ->
    case runParser (isChar open) tokens of
        Just (l, rest) -> case runParser (parseMany parseSExpr) rest of
            Just (x, xs) -> case runParser (isChar close) xs of
                Just (y, ys) -> Just (List x, ys)
                Nothing -> Nothing -- trace ("Error (line " ++ show l ++ "): no closing symbol")
            Nothing -> Nothing -- trace ("Error (line " ++ show l ++ "): failed parse content")
        Nothing -> Nothing

generalParser :: String -> [[SExpr]]
generalParser code = case runParser (parseSome (parseMany parseSExpr)) (stringToTokens code) of
    Just (x, []) -> x
    _ -> [] -- trace ("Error: Parsing failed")
