
-- IMPORTS

import Debug.Trace

-- STRUCTS

data Value =
    Numerical Int
    | Boolean Bool
    | Insts [Instruction]
    deriving (Show)

data Operators =
    Add
    | Sub
    | Mul
    | Div
    | Eq
    | Ne
    | Lt
    | Gt
    | Le
    | Ge
    | And
    | Or
    | Not
instance Show Operators where
    show (Add) = "Add"
    show (Sub) = "Sub"
    show (Mul) = "Mul"
    show (Div) = "Div"
    show (Eq) = "Eq"
    show (Ne) = "Ne"
    show (Lt) = "Lt"
    show (Gt) = "Gt"
    show (Le) = "Le"
    show (Ge) = "Ge"
    show (And) = "And"
    show (Or) = "Or"
    show (Not) = "Not"

data Instruction =
    Push Value
    | Pop
    | Call Operators
    | Ret
    | JumpIfFalse Int
instance Show Instruction where
    show (Push x) = "Push " ++ show x
    show (Pop) = "Pop"
    show (Call x) = "Call " ++ show x
    show (Ret) = "Ret"

type Stack = [Value]

type Insts = [Instruction]

-- FUNCTIONS

jumpTo :: Int -> Insts -> Stack -> Either String Value
jumpTo 0 insts stack = exec insts stack
jumpTo _ [] stack = Left "jumped out of bounds"
jumpTo n insts stack = jumpTo (n - 1) (tail insts) stack

exec :: Insts -> Stack -> Either String Value
exec [] (x:xs) = Left "finished without ret"
exec ((Push x):xs) stack = exec xs (x:stack)
exec ((Pop):xs) stack = case stack of
    (_:ss) -> exec xs ss
    _ -> Left "Nothing to pop"
exec ((Call Add):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec xs ((Numerical (x + y)):stack)
    (_:_:_) -> Left "Add need Numericals"
    _ -> Left "Add need two arguments"
exec ((Call Sub):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec xs ((Numerical (x - y)):stack)
    (_:_:_) -> Left "Sub need Numericals"
    _ -> Left "Sub need two arguments"
exec ((Call Mul):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec xs ((Numerical (x * y)):stack)
    (_:_:_) -> Left "Mul need Numericals"
    _ -> Left "Mul need two arguments"
exec ((Call Div):xs) stack = case stack of
    (Numerical _:Numerical 0:_) -> Left "Division by zero"
    (Numerical x:Numerical y:stack) -> exec xs ((Numerical (x `div` y)):stack)
    (_:_:_) -> Left "Div need Numericals"
    _ -> Left "Div need two arguments"
exec ((Call Eq):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec xs ((Boolean (x == y)):stack)
    (_:_:_) -> Left "Eq need Numericals"
    _ -> Left "Eq need two arguments"
exec ((Call Ne):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec xs ((Boolean (x /= y)):stack)
    (_:_:_) -> Left "Ne need Numericals"
    _ -> Left "Ne need two arguments"
exec ((Call Lt):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec xs ((Boolean (x < y)):stack)
    (_:_:_) -> Left "Lt need Numericals"
    _ -> Left "Lt need two arguments"
exec ((Call Gt):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec xs ((Boolean (x > y)):stack)
    (_:_:_) -> Left "Gt need Numericals"
    _ -> Left "Gt need two arguments"
exec ((Call Le):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec xs ((Boolean (x <= y)):stack)
    (_:_:_) -> Left "Le need Numericals"
    _ -> Left "Le need two arguments"
exec ((Call Ge):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec xs ((Boolean (x >= y)):stack)
    (_:_:_) -> Left "Ge need Numericals"
    _ -> Left "Ge need two arguments"
exec ((Call And):xs) stack = case stack of
    (Boolean x:Boolean y:stack) -> exec xs ((Boolean (x && y)):stack)
    (_:_:_) -> Left "And need Booleans"
    _ -> Left "And need two arguments"
exec ((Call Or):xs) stack = case stack of
    (Boolean x:Boolean y:stack) -> exec xs ((Boolean (x || y)):stack)
    (_:_:_) -> Left "Or need Booleans"
    _ -> Left "Or need two arguments"
exec ((Call Not):xs) stack = case stack of
    (Boolean x:stack) -> exec xs ((Boolean (not x)):stack)
    (_:_) -> Left "Not need Booleans"
    _ -> Left "Not need one argument"
exec ((JumpIfFalse n):xs) stack = case stack of
    (Boolean False:stack) -> if (n >= 0) then (jumpTo (n xs stack)) else Left "Negative jumps"
    (Boolean True:stack) -> exec xs stack
    _ -> Left "JumpIfFalse need Boolean"
exec (Ret:_) stack = case stack of
    (x:_) -> Right x
    _ -> Left "Nothing to return"
exec _ _ = Left "fail"
