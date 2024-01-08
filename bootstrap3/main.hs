
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
    | PushEnv String
instance Show Instruction where
    show (Push x) = "Push " ++ show x
    show (Pop) = "Pop"
    show (Call) = "Call"
    show (Ret) = "Ret"

type Stack = [Value]

type Insts = [Instruction]

type Env = [(String, Value)]

-- FUNCTIONS

jumpTo :: Args -> Int -> Insts -> Stack -> Env -> Either String Value
jumpTo args 0 insts stack env = exec args insts stack env
jumpTo _ _ [] stack _ = Left "jumped out of bounds"
jumpTo args n insts stack env = jumpTo args (n - 1) (tail insts) stack env


exec :: Args -> Insts -> Stack -> Env -> Either String Value
exec args [] (x:xs) _ = Left "finished without ret"
exec args ((Push x):xs) stack env = exec args xs (x:stack) env
exec args ((Pop):xs) stack env = case stack of
    (_:ss) -> exec args xs ss env
    _ -> Left "Nothing to pop"
exec args ((Call):xs) stack env = case stack of
    (Operator op:stack') -> executeOperator args xs op stack' env
    (Function insts:stack') -> exec args insts stack' env
    _ -> Left "Call needs an Operator or Function on the stack"
exec args ((JumpIfFalse n):xs) stack env = case stack of
    (Boolean False:stack') -> if n >= 0 then jumpTo args n xs stack' env else Left "Negative jumps"
    (Boolean True:stack') -> exec args xs stack' env
    _ -> Left "JumpIfFalse needs Boolean"
exec args (Ret:_) stack env = case stack of
    (x:_) -> Right x
    _ -> Left "Nothing to return"
exec args ((PushArg index):xs) stack env = case args of
    [] -> Left "No arguments available"
    _ -> exec args xs ((args !! index) : stack) env
exec args ((PushEnv var):xs) stack env = case lookup var env of
    Just value -> exec args xs (value : stack) env
    Nothing -> Left $ "Variable '" ++ var ++ "' not found in environment"
exec _ _ _ _ = Left "fail"

executeOperator :: Args -> Insts -> Operators -> Stack -> Env -> Either String Value
executeOperator args xs op stack env =
    case op of
        Add -> executeBinaryOp (\x y -> Right (Numerical (x + y))) args xs stack env
        Sub -> executeBinaryOp (\x y -> Right (Numerical (x - y))) args xs stack env
        Mul -> executeBinaryOp (\x y -> Right (Numerical (x * y))) args xs stack env
        Div -> executeDivOp args xs stack env
        Eq  -> executeBinaryOp (\x y -> Right (Boolean (x == y))) args xs stack env
        Ne  -> executeBinaryOp (\x y -> Right (Boolean (x /= y))) args xs stack env
        Lt  -> executeBinaryOp (\x y -> Right (Boolean (x < y))) args xs stack env
        Gt  -> executeBinaryOp (\x y -> Right (Boolean (x > y))) args xs stack env
        Le  -> executeBinaryOp (\x y -> Right (Boolean (x <= y))) args xs stack env
        Ge  -> executeBinaryOp (\x y -> Right (Boolean (x >= y))) args xs stack env
        And -> executeBooleanOp (&&) args xs stack env
        Or  -> executeBooleanOp (||) args xs stack env

executeDivOp :: Args -> Insts -> Stack -> Env -> Either String Value
executeDivOp args xs stack env = case stack of
    (Numerical 0:Numerical _:_) -> Left "Division by zero"
    (Numerical y:Numerical x:stack') -> exec args xs (Numerical (x `div` y) : stack') env
    _ -> Left "Div operation needs two Numericals"

executeBinaryOp :: (Int -> Int -> Either String Value) -> Args -> Insts -> Stack -> Env -> Either String Value
executeBinaryOp op args xs stack env = case stack of
    (Numerical x:Numerical y:stack') -> op x y >>= \result -> exec args xs (result : stack') env
    _ -> Left "Binary operation needs two Numericals"

executeBooleanOp :: (Bool -> Bool -> Bool) -> Args -> Insts -> Stack -> Env -> Either String Value
executeBooleanOp op args xs stack env = case stack of
    (Boolean x:Boolean y:stack') -> exec args xs (Boolean (op x y) : stack') env
    _ -> Left "Boolean operation needs two Booleans"

main :: IO ()
main = do
    let arguments = [Numerical 5]

    let factFunction =
            [PushArg 0,
             Push (Numerical 1),
             Push (Operator Ge),
             Call,
             JumpIfFalse 2,
             Push (Numerical 1),
             Ret,
             PushArg 0,
             Push (Numerical 1),
             Push (Operator Sub),
             PushEnv "fact",
             Call,
             PushArg 0,
             Push (Operator Mul),
             Call,
             Ret]

    let env = [("fact", Function factFunction)]

    let instructions =
            [PushArg 0,
             PushEnv "fact",
             Call,
             Ret]

    case exec arguments instructions [] env of
        Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
        Right result -> print result
