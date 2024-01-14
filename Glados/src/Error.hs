module Error (
    sendError,
    errorParams,
    exceptionMessage
) where

-- IMPORTS

import System.Exit
import System.IO
import Control.Exception

-- FUNCTIONS

sendError :: Int -> String -> IO ()
sendError code message = hPutStrLn stderr message >>
    exitWith (ExitFailure code)

exceptionMessage :: IOException -> IO String
exceptionMessage _ = hPutStrLn stderr "Wrong filepath" >>
    exitWith (ExitFailure 84)

errorParams :: IO ()
errorParams = hPutStrLn stderr "USAGE: ./glados <filepath>" >>
    exitWith (ExitFailure 84)
