module Tools (extractFromFile) where

-- IMPORTS

import Error
import Control.Exception (catch)

-- FUNCTIONS

extractFromFile :: String -> IO String
extractFromFile filepath = readFile filepath `catch` exceptionMessage
