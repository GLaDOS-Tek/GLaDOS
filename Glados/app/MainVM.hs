module Main (
    main
) where

import System.Environment (getArgs)
import Structs
import VM
import Tools (extractFromFile)
import Bytecode (writeBinaryToFile, readBinaryFromFile)

main :: IO ()
main = do
    args <- getArgs
    (env, inst) <- readBinaryFromFile (args !! 0)
    case exec [] inst [] env [] [] of
        Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
        Right (result) -> print result
