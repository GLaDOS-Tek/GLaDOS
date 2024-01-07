
-- IMPORTS

import Debug.Trace

-- STRUCTS

type Args = [Value]

data Value =
    Numerical Int
    | Boolean Bool
    | Operator Operators
    | Insts [Instruction]
    | Function Insts
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
    | Call
    | Ret
    | JumpIfFalse Int
    | PushArg Int
instance Show Instruction where
    show (Push x) = "Push " ++ show x
    show (Pop) = "Pop"
    show (Call) = "Call"
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
exec args ((Call):xs) stack = case stack of
    (Operator op:stack') -> executeOperator args xs op stack'
    (Function insts:stack') -> exec args insts stack'
    _ -> Left "Call needs an Operator or Function on the stack"
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

executeOperator :: Args -> Insts -> Operators -> Stack -> Either String Value
executeOperator args xs op stack =
    case op of
        Add -> executeBinaryOp (\x y -> Right (Numerical (x + y))) args xs stack
        Sub -> executeBinaryOp (\x y -> Right (Numerical (x - y))) args xs stack
        Mul -> executeBinaryOp (\x y -> Right (Numerical (x * y))) args xs stack
        Div -> executeDivOp args xs stack
        Eq  -> executeBinaryOp (\x y -> Right (Boolean (x == y))) args xs stack
        Ne  -> executeBinaryOp (\x y -> Right (Boolean (x /= y))) args xs stack
        Lt  -> executeBinaryOp (\x y -> Right (Boolean (x < y))) args xs stack
        Gt  -> executeBinaryOp (\x y -> Right (Boolean (x > y))) args xs stack
        Le  -> executeBinaryOp (\x y -> Right (Boolean (x <= y))) args xs stack
        Ge  -> executeBinaryOp (\x y -> Right (Boolean (x >= y))) args xs stack
        And -> executeBooleanOp (&&) args xs stack
        Or  -> executeBooleanOp (||) args xs stack

executeDivOp :: Args -> Insts -> Stack -> Either String Value
executeDivOp args xs stack = case stack of
    (Numerical 0:Numerical _:_) -> Left "Division by zero"
    (Numerical y:Numerical x:stack') -> exec args xs (Numerical (x `div` y) : stack')
    _ -> Left "Div operation needs two Numericals"

executeBinaryOp :: (Int -> Int -> Either String Value) -> Args -> Insts -> Stack -> Either String Value
executeBinaryOp op args xs stack = case stack of
    (Numerical x:Numerical y:stack') -> op x y >>= \result -> exec args xs (result : stack')
    _ -> Left "Binary operation needs two Numericals"

executeBooleanOp :: (Bool -> Bool -> Bool) -> Args -> Insts -> Stack -> Either String Value
executeBooleanOp op args xs stack = case stack of
    (Boolean x:Boolean y:stack') -> exec args xs (Boolean (op x y) : stack')
    _ -> Left "Boolean operation needs two Booleans"

main :: IO ()
main = do
    let arguments = [Numerical (-42)]
    let absCodeFunction =
            [ PushArg 0
            , Push (Numerical 0)
            , Push (Operator Lt)
            , Call
            , JumpIfFalse 2
            , PushArg 0
            , Ret
            , PushArg 0
            , Push (Numerical (-1))
            , Push (Operator Mul)
            , Call
            , Ret
            ]

    let instructions =
            [ Push (Numerical (-42))
            , Push (Function absCodeFunction)
            , Call
            , Ret
            ]

    case exec arguments instructions [] of
        Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
        Right result -> print result