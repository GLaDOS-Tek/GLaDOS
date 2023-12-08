import Text.Read

type Parser a = String -> Maybe (a, String)

data SExpr = IntExpr Int Int -- second int is the line
    | StrExpr String Int
    | ExprList [SExpr]
    deriving (Show, Read)

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

parseUInt :: Parser Int -- parse an unsigned Int
parseUInt s = case (parseSome(parseAnyChar ['0'..'9']) s) of
    Just (x, xs) -> case (readMaybe x :: Maybe Int) of
        Just nb -> Just (nb, xs)
        Nothing -> Nothing
    Nothing -> Nothing
    where
        parsed = parseSome (parseAnyChar ['0'..'9']) s

parseInt :: Parser Int -- parse a signed Int
parseInt ('-':xs) = case parseUInt xs of
    Just (a, b) -> Just ((-a), b)
    Nothing -> Nothing
parseInt s = parseUInt s

parsePair :: Parser a -> Parser (a,a)
parsePair p ('(':xs) = case (p xs) of
    Just (a, (' ':as)) -> case (p as) of
        Just (b, (')':bs)) -> Just ((a, b), bs)
        _ -> Nothing
    _ -> Nothing
parsePair _ _ = Nothing

parseListElement :: Parser a -> Parser [a]
parseListElement p s = case p s of
    Just (a, (' ':as)) -> case parseListElement p as of
        Just (b, bs) -> Just (a:b, bs)
        _ -> Nothing
    Just (a, (')':as)) -> Just ([a], as)
    _ -> Nothing

parseList :: Parser a -> Parser [a]
parseList p ('(':xs) = parseListElement p xs
parseList _ _ = Nothing