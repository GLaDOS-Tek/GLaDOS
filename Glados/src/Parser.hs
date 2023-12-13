module Parser (generalParser) where

-- IMPORTS

import Text.Read
import Debug.Trace

-- CUSTOM TYPES

type Parser a = String -> Maybe (a, String)

data SExpr = IntExpr Int Int -- second int is the line
    | StrExpr String Int
    | SymbolExpr String Int
    | ExprList [SExpr]
    deriving (Show, Read)

-- GLOBAL VAR

ascii_char = enumFromTo ' ' '~'
banned_char = [' ', '(', ')', '\"']
valid_char = filter (`notElem` banned_char) ascii_char

-- FUNCTIONS

parseChar :: Char -> Parser Char
parseChar c (x:xs)
    | c == x = Just (c, xs)
    | otherwise = Nothing
parseChar _ _ = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar (x:xs) str = parseOr (parseChar x) (parseAnyChar xs) str
parseAnyChar _ _ = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 s = case p1 s of
    Just (x, xs) -> Just (x, xs)
    Nothing -> p2 s

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 s = case p1 s of
    Just (x, xs) -> case p2 xs of
        Just (y, ys) -> Just ((x, y), ys)
        Nothing -> Nothing
    Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith function p1 p2 s = case parseAnd p1 p2 s of
    Just ((x, y), xs) -> Just (function x y, xs)
    Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p s = case p s of
    Just (x, xs) -> case parseMany p xs of
        Just (y, ys) -> Just (x:y, ys)
        Nothing -> Just ([x], xs)
    Nothing -> Just ([], s)

parseSome :: Parser a -> Parser [a]
parseSome p s = case p s of
    Just (x, xs) -> case parseMany p xs of
        Just (y, ys) -> Just ((x:y), ys)
        Nothing -> Just ([x], xs)
    Nothing -> Nothing

parsePair :: Parser a -> Parser (a,a)
parsePair p ('(':xs) = case (p xs) of
    Just (a, (' ':as)) -> case (p as) of
        Just (b, (')':bs)) -> Just ((a, b), bs)
        _ -> Nothing
    _ -> Nothing
parsePair _ _ = Nothing

readUInt :: String -> String -> Maybe (Int, String)
readUInt s xs = case (readMaybe s :: Maybe Int) of
        Just nb -> Just (nb, xs)
        Nothing -> Nothing

parseUInt :: Parser Int -- parse an unsigned Int
parseUInt s = case (parseSome(parseAnyChar ['0'..'9']) s) of
    Just (x, (' ':xs)) -> readUInt x (' ':xs)
    Just (x, (')':xs)) -> readUInt x (')':xs)
    Just (x, "") -> readUInt x ""
    _ -> Nothing
    where
        parsed = parseSome (parseAnyChar ['0'..'9']) s

parseInt :: Parser Int -- parse a signed Int
parseInt ('-':xs) = case parseUInt xs of
    Just (a, b) -> Just ((-a), b)
    Nothing -> Nothing
parseInt s = parseUInt s

getBiggerLine :: Int -> SExpr -> Int
getBiggerLine line (SymbolExpr _ newLine) = newLine
getBiggerLine line (IntExpr _ newLine) = newLine
getBiggerLine line (StrExpr _ newLine) = newLine
getBiggerLine line (ExprList _) = line

parseListElement :: Int -> Parser [SExpr]
parseListElement line (' ':xs) = parseListElement line xs --prevent to give " )" to general parser
parseListElement line (')':xs) = Just ([], xs)
parseListElement line str = case generalParser line str of
    Just (a, (')':as)) -> Just ([a], as)
    Just (a, as) -> case parseListElement (getBiggerLine line a) as of
        Just (b, bs) -> Just ([a] ++ b, bs)
        Nothing -> Nothing
    _ -> Nothing

parseList :: Int -> Parser [SExpr]
parseList line ('(':xs) = parseListElement line xs
parseList _ s = Nothing

parseString :: Int -> Parser SExpr
parseString l "" = Nothing
parseString l (' ':xs) = Nothing
parseString l ('\"':xs) = case parseSome (parseAnyChar (filter (/= '\"') ascii_char)) xs of
    Just (a, '\"':as) -> Just (StrExpr a l, as)
    _ -> Nothing
parseString l s = case parseSome (parseAnyChar valid_char) s of
    Just (a, as) -> Just (SymbolExpr a l, as)
    _ -> Nothing

generalParser :: Int -> Parser SExpr
generalParser _ (')':xs) = Nothing
generalParser line ('\n':xs) = generalParser (line + 1) xs
generalParser line ('\t':xs) = generalParser line xs
generalParser line (' ':xs) = generalParser line xs
generalParser line str = case parseInt str of
    Just (i, is) -> Just ((IntExpr i line), is)
    Nothing -> case parseString line str of
        Just (s, ss) -> Just (s, ss)
        Nothing -> case (parseList line str) of
            Just (list, ls) -> Just (ExprList list, ls)
            Nothing -> Nothing
