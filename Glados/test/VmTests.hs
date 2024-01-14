module VmTests (vmTests) where

import Test.Hspec
import Structs (Value(..), Operators(..), Instruction(..))

vmTests :: IO ()
vmTests = hspec $ do
    describe "Test the exec function" $ do
        it "executes a simple addition" $ do
            let instructions = [Push (Numerical 2), Push (Numerical 3), Push (Operator Add), Ret]
            let result = exec [] instructions [] [] [] []
            result `shouldBe` Right (Numerical 5)

        it "handles division by zero" $ do
            let instructions = [Push (Numerical 2), Push (Numerical 0), Push (Operator Div), Ret]
            let result = exec [] instructions [] [] [] []
            result `shouldBe` Left "Division by zero"

        it "executes a function call factorial" $ do
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

            let instructions = [Push (Numerical 5), Push (Function factFunction), Call, Ret]
            let result = exec [] instructions [] [] [] []
            result `shouldBe` Right (Numerical 120)
        it "executes a function call from environment" $ do
            let sumFunction =
                    [Push (Numerical 0),
                     PushArg 0,
                     Push (Operator Eq),
                     JumpIfFalse 2,
                     Ret,
                     PushArg 0,
                     Push (Operator Add),
                     Call,
                     Ret]

            let env = [("sum", Function sumFunction)]

            let instructions = [Push (Numerical 5), PushEnv "sum", Call, Ret]
            let result = exec [] instructions [] env [] []
            result `shouldBe` Right (Numerical 15)

        