module Parser (generalParser, SExpr(..), exprParser, parseChar, parseMany, parseSome, parseInt, parseList, parseString) where

-- IMPORTS

import Text.Read
import Debug.Trace

-- CUSTOM TYPES

type Parser a = String -> Maybe (a, String)

data SExpr = IntExpr Int Int -- second int is the line
    | StrExpr String Int
    | SymbolExpr String Int
    | ExprList [SExpr]
    deriving (Show, Read, Eq)

-- GLOBAL VAR

ascii_char :: [Char]
ascii_char = enumFromTo ' ' '~'

banned_char :: [Char]
banned_char = [' ', '(', ')', '\"']

valid_char :: [Char]
valid_char = filter (`notElem` banned_char) ascii_char

-- FUNCTIONS

parseChar :: Char -> Parser Char
parseChar c (x:xs)
    | c == x = Just (c, xs)
    | otherwise = Nothing
parseChar _ _ = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 s = case p1 s of
    Just (x, xs) -> Just (x, xs)
    Nothing -> p2 s

parseAnyChar :: String -> Parser Char
parseAnyChar (x:xs) str = parseOr (parseChar x) (parseAnyChar xs) str
parseAnyChar _ _ = Nothing

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

readUInt :: String -> String -> Maybe (Int, String)
readUInt s xs = case (readMaybe s :: Maybe Int) of
        Just nb -> Just (nb, xs)
        Nothing -> Nothing

parseUInt :: Parser Int -- parse an unsigned Int
parseUInt s = case (parseSome(parseAnyChar ['0'..'9']) s) of
    Just (x, (' ':xs)) -> readUInt x (' ':xs)
    Just (x, (')':xs)) -> readUInt x (')':xs)
    Just (x, ('\n':xs)) -> readUInt x ('\n':xs)
    Just (x, ('\t':xs)) -> readUInt x ('\t':xs)
    Just (x, "") -> readUInt x ""
    _ -> Nothing

parseInt :: Parser Int -- parse a signed Int
parseInt ('-':xs) = case parseUInt xs of
    Just (a, b) -> Just ((-a), b)
    Nothing -> Nothing
parseInt s = parseUInt s

getBiggerLine :: Int -> SExpr -> Int
getBiggerLine _ (SymbolExpr _ newLine) = newLine
getBiggerLine _ (IntExpr _ newLine) = newLine
getBiggerLine _ (StrExpr _ newLine) = newLine
getBiggerLine line (ExprList list) = getBiggerLine line (last list)

parseList :: Int -> Parser [SExpr]
parseList line ('(':xs) = generalParser line xs
parseList _ _ = Nothing

parseString :: Int -> Parser SExpr
parseString _ "" = Nothing
parseString _ (' ':_) = Nothing
parseString l ('\"':xs) = case parseSome (parseAnyChar (filter (/= '\"') ascii_char)) xs of
    Just (a, '\"':as) -> Just (StrExpr a l, as)
    _ -> Nothing
parseString l s = case parseSome (parseAnyChar valid_char) s of
    Just (a, as) -> Just (SymbolExpr a l, as)
    _ -> Nothing

exprParser :: Int -> Parser SExpr
exprParser line str = case parseInt str of
    Just (i, is) -> Just ((IntExpr i line), is)
    Nothing -> case parseString line str of
        Just (s, ss) -> Just (s, ss)
        Nothing -> case (parseList line str) of
            Just (list, ls) -> Just (ExprList list, ls)
            Nothing -> trace ("exprParser failed :\n-->" ++ str ++ "<--") Nothing

generalParser :: Int -> Parser [SExpr]
generalParser _ "" = Just ([], "")
generalParser _ (')':xs) = Just ([], xs)
generalParser line ('\n':xs) = generalParser (line + 1) xs
generalParser line ('\t':xs) = generalParser line xs
generalParser line (' ':xs) = generalParser line xs
generalParser line str = case exprParser line str of
    Just (a, as) -> case generalParser (getBiggerLine line a) as of
        Just (b, bs) -> Just ([a] ++ b, bs)
        Nothing -> Nothing
    _ -> Nothing
