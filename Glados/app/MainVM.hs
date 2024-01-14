module Main (
    main
) where

import Structs
import VM

main :: IO ()
main = do
    let factFunction =
         [PushArg 0,
         Push (Numerical 1),
         Push (Operator Eq),
         Call,
         JumpIfFalse 2,
         Push (Numerical 1),
         Ret,
         PushArg 0,
         Push (Numerical 1),
         Push (Operator Sub),
         Call,
         Push (Function factFunction),
         Call,
         PushArg 0,
         Push (Operator Mul),
         Call,
         Ret]

    let env = [("fact", Function factFunction)]

    let instructions =
            [Push (Numerical 5),
             PushEnv "fact",
             Call,
             Ret]

    case exec [] instructions [] env [] [] of
        Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
        Right result -> print result
