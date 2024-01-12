
-- IMPORTS

import Debug.Trace

-- STRUCTS

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

type Args = [Value]

type Env = [(String, Value)]

type FuncInsts = [Insts]

type ListArgs = [Args]

-- FUNCTIONS

jumpTo :: Args -> Int -> Insts -> Stack -> Env -> FuncInsts -> ListArgs -> Either String Value
jumpTo args 0 insts stack env funcInsts listArgs = exec args insts stack env funcInsts listArgs
jumpTo _ _ [] stack _ _ _= Left "jumped out of bounds"
jumpTo args n insts stack env funcInsts listArgs = jumpTo args (n - 1) (tail insts) stack env funcInsts listArgs

execFunction :: Args -> Insts -> Stack -> Env -> FuncInsts -> ListArgs -> Either String Value
execFunction args insts stack env funcInsts listArgs =
    case stack of
        [] -> exec args insts [] env funcInsts listArgs
        (lastValue:restOfStack) ->
            execFunction (lastValue : args) insts restOfStack env funcInsts listArgs

exec :: Args -> Insts -> Stack -> Env -> FuncInsts -> ListArgs -> Either String Value
exec args [] (x:xs) _ [] _ = Right x
exec args [] [] stack (funcInst:funcInsts) (lastArg:listArgs) = exec lastArg funcInst [] stack funcInsts listArgs
exec args ((Push x):xs) stack env funcInsts listArgs = exec args xs (x:stack) env funcInsts listArgs
exec args (Pop:xs) stack env funcInsts listArgs = case stack of
    (_:ss) -> exec args xs ss env funcInsts listArgs
    _ -> Left "Nothing to pop"
exec args (Call:xs) stack env funcInsts listArgs = case stack of
    (Operator op:stack') -> executeOperator args xs op stack' env funcInsts listArgs
    (Function insts:stack') -> execFunction args insts stack' env (xs : funcInsts) (args:listArgs)
    _ -> Left "Call needs an Operator or Function on the stack"
exec args ((JumpIfFalse n):xs) stack env funcInsts listArgs = case stack of
    (Boolean False:stack') -> if n >= 0 then jumpTo args n xs stack' env funcInsts listArgs else Left "Negative jumps"
    (Boolean True:stack') -> exec args xs stack' env funcInsts listArgs
    _ -> Left "JumpIfFalse needs Boolean"
exec args (Ret:_) stack env funcInsts listArgs =
    case funcInsts of
        [] -> case stack of
            [] -> Left "No function instance or value to return to"
            x:_ -> Right x
        lastFuncInst:restFuncInsts ->
            let (lastArg:restListArgs) = listArgs
            in exec lastArg lastFuncInst stack env restFuncInsts restListArgs
exec args ((PushArg index):xs) stack env funcInsts listArgs = case args of
    [] -> Left "No arguments available"
    _ -> exec args xs ((args !! index) : stack) env funcInsts listArgs
exec args ((PushEnv var):xs) stack env funcInsts listArgs = case lookup var env of
    Just value -> exec args xs (value : stack) env funcInsts listArgs
    Nothing -> Left $ "Variable '" ++ var ++ "' not found in environment"
exec _ _ _ _ _ _ = Left "fail"

executeOperator :: Args -> Insts -> Operators -> Stack -> Env -> FuncInsts -> ListArgs -> Either String Value
executeOperator args xs op stack env funcInsts listArgs =
    case op of
        Add -> executeBinaryOp (\x y -> Right (Numerical (x + y))) args xs stack env funcInsts listArgs
        Sub -> executeBinaryOp (\x y -> Right (Numerical (x - y))) args xs stack env funcInsts listArgs
        Mul -> executeBinaryOp (\x y -> Right (Numerical (x * y))) args xs stack env funcInsts listArgs
        Div -> executeDivOp args xs stack env funcInsts listArgs
        Eq  -> executeBinaryOp (\x y -> Right (Boolean (x == y))) args xs stack env funcInsts listArgs
        Ne  -> executeBinaryOp (\x y -> Right (Boolean (x /= y))) args xs stack env funcInsts listArgs
        Lt  -> executeBinaryOp (\x y -> Right (Boolean (x < y))) args xs stack env funcInsts listArgs
        Gt  -> executeBinaryOp (\x y -> Right (Boolean (x > y))) args xs stack env funcInsts listArgs
        Le  -> executeBinaryOp (\x y -> Right (Boolean (x <= y))) args xs stack env funcInsts listArgs
        Ge  -> executeBinaryOp (\x y -> Right (Boolean (x >= y))) args xs stack env funcInsts listArgs
        And -> executeBooleanOp (&&) args xs stack env funcInsts listArgs
        Or  -> executeBooleanOp (||) args xs stack env funcInsts listArgs

executeDivOp :: Args -> Insts -> Stack -> Env -> FuncInsts -> ListArgs-> Either String Value
executeDivOp args xs stack env funcInsts listArgs = case stack of
    (Numerical 0:Numerical _:_) -> Left "Division by zero"
    (Numerical y:Numerical x:stack') -> exec args xs (Numerical (x `div` y) : stack') env funcInsts listArgs
    _ -> Left "Div operation needs two Numericals"

executeBinaryOp :: (Int -> Int -> Either String Value) -> Args -> Insts -> Stack -> Env -> FuncInsts -> ListArgs -> Either String Value
executeBinaryOp op args xs stack env funcInsts listArgs = case stack of
    (Numerical x:Numerical y:stack') -> op x y >>= \result -> exec args xs (result : stack') env funcInsts listArgs
    _ -> Left "Binary operation needs two Numericals"

executeBooleanOp :: (Bool -> Bool -> Bool) -> Args -> Insts -> Stack -> Env -> FuncInsts -> ListArgs -> Either String Value
executeBooleanOp op args xs stack env funcInsts listArgs = case stack of
    (Boolean x:Boolean y:stack') -> exec args xs (Boolean (op x y) : stack') env funcInsts listArgs
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
            PushEnv "fun2",
            Call,
            Ret]

    let env = [("fun", Function func), ("fun2", Function func2)]

    let instructions =
            [Push (Numerical 5),
             Push (Function factFunction),
             Call,
             Ret]

    case exec [] instructions [] env [] [] of
        Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
        Right result -> print result
