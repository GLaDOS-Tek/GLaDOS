
-- IMPORTS

import Debug.Trace

-- STRUCTS

type Args = [Value]

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
    | PushArg Int
instance Show Instruction where
    show (Push x) = "Push " ++ show x
    show (Pop) = "Pop"
    show (Call x) = "Call " ++ show x
    show (Ret) = "Ret"

type Stack = [Value]

type Insts = [Instruction]

-- FUNCTIONS

jumpTo :: Args -> Int -> Insts -> Stack -> Either String Value
jumpTo args 0 insts stack = exec args insts stack
jumpTo _ _ [] stack = Left "jumped out of bounds"
jumpTo args n insts stack = jumpTo args (n - 1) (tail insts) stack

exec :: Args -> Insts -> Stack -> Either String Value
exec args [] (x:xs) = Left "finished without ret"
exec args ((Push x):xs) stack = exec args xs (x:stack)
exec args ((Pop):xs) stack = case stack of
    (_:ss) -> exec args xs ss
    _ -> Left "Nothing to pop"
exec args ((Call Add):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec args xs ((Numerical (x + y)):stack)
    (_:_:_) -> Left "Add need Numericals"
    _ -> Left "Add need two arguments"
exec args ((Call Sub):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec args xs ((Numerical (x - y)):stack)
    (_:_:_) -> Left "Sub need Numericals"
    _ -> Left "Sub need two arguments"
exec args ((Call Mul):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec args xs ((Numerical (x * y)):stack)
    (_:_:_) -> Left "Mul need Numericals"
    _ -> Left "Mul need two arguments"
exec args ((Call Div):xs) stack = case stack of
    (Numerical _:Numerical 0:_) -> Left "Division by zero"
    (Numerical x:Numerical y:stack) -> exec args xs ((Numerical (x `div` y)):stack)
    (_:_:_) -> Left "Div need Numericals"
    _ -> Left "Div need two arguments"
exec args ((Call Eq):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec args xs ((Boolean (x == y)):stack)
    (_:_:_) -> Left "Eq need Numericals"
    _ -> Left "Eq need two arguments"
exec args ((Call Ne):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec args xs ((Boolean (x /= y)):stack)
    (_:_:_) -> Left "Ne need Numericals"
    _ -> Left "Ne need two arguments"
exec args ((Call Lt):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec args xs ((Boolean (x < y)):stack)
    (_:_:_) -> Left "Lt need Numericals"
    _ -> Left "Lt need two arguments"
exec args ((Call Gt):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec args xs ((Boolean (x > y)):stack)
    (_:_:_) -> Left "Gt need Numericals"
    _ -> Left "Gt need two arguments"
exec args ((Call Le):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec args xs ((Boolean (x <= y)):stack)
    (_:_:_) -> Left "Le need Numericals"
    _ -> Left "Le need two arguments"
exec args ((Call Ge):xs) stack = case stack of
    (Numerical x:Numerical y:stack) -> exec args xs ((Boolean (x >= y)):stack)
    (_:_:_) -> Left "Ge need Numericals"
    _ -> Left "Ge need two arguments"
exec args ((Call And):xs) stack = case stack of
    (Boolean x:Boolean y:stack) -> exec args xs ((Boolean (x && y)):stack)
    (_:_:_) -> Left "And need Booleans"
    _ -> Left "And need two arguments"
exec args ((Call Or):xs) stack = case stack of
    (Boolean x:Boolean y:stack) -> exec args xs ((Boolean (x || y)):stack)
    (_:_:_) -> Left "Or need Booleans"
    _ -> Left "Or need two arguments"
exec args ((Call Not):xs) stack = case stack of
    (Boolean x:stack) -> exec args xs ((Boolean (not x)):stack)
    (_:_) -> Left "Not need Booleans"
    _ -> Left "Not need one argument"
exec args ((JumpIfFalse n):xs) stack = case stack of
    (Boolean False:stack) -> if n >= 0 then jumpTo args n xs stack else Left "Negative jumps"
    (Boolean True:stack) -> exec args xs stack
    _ -> Left "JumpIfFalse needs Boolean"
exec args (Ret:_) stack = case stack of
    (x:_) -> Right x
    _ -> Left "Nothing to return"
exec args ((PushArg index):xs) stack = case args of
    [] -> Left "No arguments available"
    _ -> exec args xs ((args !! index) : stack)
exec _ _ _ = Left "fail"

main :: IO ()
main = do
    let arguments = [Numerical 42]
    let instructions =
            [ PushArg 0, Push (Numerical 0), Call Le, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (Numerical (-1)), Call Mul, Ret]
    case exec arguments instructions [] of
        Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
        Right result -> print result