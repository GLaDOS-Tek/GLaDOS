import ParserTests (parserTests)
import VmTests (vmTests)

main :: IO ()
main = do
    parserTests
    vmTests
