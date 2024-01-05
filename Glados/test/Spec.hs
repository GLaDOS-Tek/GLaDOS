import ParserTests (parserTests)
import AstTests (astTests)
import EvalTests (evalTests)

main :: IO ()
main = do
    parserTests
    astTests
    evalTests