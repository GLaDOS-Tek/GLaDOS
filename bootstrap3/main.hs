
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

type FuncInsts = [Insts]

-- FUNCTIONS

jumpTo :: Args -> Int -> Insts -> Stack -> Env -> FuncInsts -> Either String Value
jumpTo args 0 insts stack env funcInsts = exec args insts stack env funcInsts
jumpTo _ _ [] stack _ _= Left "jumped out of bounds"
jumpTo args n insts stack env funcInsts= jumpTo args (n - 1) (tail insts) stack env funcInsts

execFunction :: Args -> Insts -> Stack -> Env -> FuncInsts -> Either String Value
execFunction args insts stack env funcInsts =
    case stack of
        [] -> exec args insts [] env funcInsts
        (lastValue:restOfStack) ->
            execFunction (lastValue:args) insts restOfStack env funcInsts

exec :: Args -> Insts -> Stack -> Env -> FuncInsts -> Either String Value
exec args [] (x:xs) _ [] = Right x
exec args [] [] _ (funcInst:funcInsts) = exec args funcInst [] [] funcInsts
exec args ((Push x):xs) stack env funcInsts = exec args xs (x:stack) env funcInsts
exec args (Pop:xs) stack env funcInsts = case stack of
    (_:ss) -> exec args xs ss env funcInsts
    _ -> Left "Nothing to pop"
exec args (Call:xs) stack env funcInsts = case stack of
    (Operator op:stack') -> executeOperator args xs op stack' env funcInsts
    (Function insts:stack') -> execFunction args insts stack' env (xs : funcInsts)
    _ -> Left "Call needs an Operator or Function on the stack"
exec args ((JumpIfFalse n):xs) stack env funcInsts = case stack of
    (Boolean False:stack') -> if n >= 0 then jumpTo args n xs stack' env funcInsts else Left "Negative jumps"
    (Boolean True:stack') -> exec args xs stack' env funcInsts
    _ -> Left "JumpIfFalse needs Boolean"
exec args (Ret:_) stack env funcInsts = case funcInsts of
    [] -> case stack of
        [] -> Left "No function instance or value to return to"
        x:_ -> Right x
    lastFuncInst:restFuncInsts -> exec args lastFuncInst stack env restFuncInsts
exec args ((PushArg index):xs) stack env funcInsts = case args of
    [] -> Left "No arguments available"
    _ -> exec args xs ((args !! index) : stack) env funcInsts
exec args ((PushEnv var):xs) stack env funcInsts = case lookup var env of
    Just value -> exec args xs (value : stack) env funcInsts
    Nothing -> Left $ "Variable '" ++ var ++ "' not found in environment"
exec _ _ _ _ _ = Left "fail"


executeOperator :: Args -> Insts -> Operators -> Stack -> Env -> FuncInsts -> Either String Value
executeOperator args xs op stack env funcInsts =
    case op of
        Add -> executeBinaryOp (\x y -> Right (Numerical (x + y))) args xs stack env funcInsts
        Sub -> executeBinaryOp (\x y -> Right (Numerical (x - y))) args xs stack env funcInsts
        Mul -> executeBinaryOp (\x y -> Right (Numerical (x * y))) args xs stack env funcInsts
        Div -> executeDivOp args xs stack env funcInsts
        Eq  -> executeBinaryOp (\x y -> Right (Boolean (x == y))) args xs stack env funcInsts
        Ne  -> executeBinaryOp (\x y -> Right (Boolean (x /= y))) args xs stack env funcInsts
        Lt  -> executeBinaryOp (\x y -> Right (Boolean (x < y))) args xs stack env funcInsts
        Gt  -> executeBinaryOp (\x y -> Right (Boolean (x > y))) args xs stack env funcInsts
        Le  -> executeBinaryOp (\x y -> Right (Boolean (x <= y))) args xs stack env funcInsts
        Ge  -> executeBinaryOp (\x y -> Right (Boolean (x >= y))) args xs stack env funcInsts
        And -> executeBooleanOp (&&) args xs stack env funcInsts
        Or  -> executeBooleanOp (||) args xs stack env funcInsts

executeDivOp :: Args -> Insts -> Stack -> Env -> FuncInsts -> Either String Value
executeDivOp args xs stack env funcInsts = case stack of
    (Numerical 0:Numerical _:_) -> Left "Division by zero"
    (Numerical y:Numerical x:stack') -> exec args xs (Numerical (x `div` y) : stack') env funcInsts
    _ -> Left "Div operation needs two Numericals"

executeBinaryOp :: (Int -> Int -> Either String Value) -> Args -> Insts -> Stack -> Env -> FuncInsts -> Either String Value
executeBinaryOp op args xs stack env funcInsts = case stack of
    (Numerical x:Numerical y:stack') -> op x y >>= \result -> exec args xs (result : stack') env funcInsts
    _ -> Left "Binary operation needs two Numericals"

executeBooleanOp :: (Bool -> Bool -> Bool) -> Args -> Insts -> Stack -> Env -> FuncInsts -> Either String Value
executeBooleanOp op args xs stack env funcInsts = case stack of
    (Boolean x:Boolean y:stack') -> exec args xs (Boolean (op x y) : stack') env funcInsts
    _ -> Left "Boolean operation needs two Booleans"

main :: IO ()
main = do
    let incrementFunction =
            [Push (Numerical 2),
             PushArg 0,
             Push (Operator Eq),
             Call,
             JumpIfFalse 2,
             PushArg 0,
             Ret,
             PushArg 0,
             Push (Numerical 1),
             Push (Operator Add),
             Call,
             Push (Function incrementFunction),
             Call,
             Ret]
    
    let factFunction =
         [PushArg 0,
         Push (Numerical 1),
         Push (Operator Eq),
         Call,
         JumpIfFalse 2,
         Push (Numerical 1),
         Ret,
         Push (Numerical 1),
         PushArg 0,
         Push (Operator Sub),
         Call,
         Push (Function factFunction),
         Call,
         PushArg 0,
         Push (Operator Mul),
         Call,
         Ret]

    let func2 =
            [PushArg 0,
            Push (Numerical 5),
            Push (Operator Add),
            Call,
            Ret]

    let func =
            [PushArg 0,
            PushArg 1,
            Push (Operator Add),
            Call,
            Push (Function func2),
            Call,
            Ret]

    let env = [("increment", Function incrementFunction)]

    let instructions =
            [Push (Numerical 5),
             Push (Numerical 2),
             Push (Function func),
             Call,
             Push (Numerical 1),
             Push (Operator Add),
             Call,
             Ret]

    case exec [] instructions [] env [] of
        Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
        Right result -> print result
