import ParserTests (parserTests)
import AstTests (astTests)

main :: IO ()
main = do
    parserTests
    astTests