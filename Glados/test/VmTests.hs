module VmTests (vmTests) where

import VM
import Test.Hspec
import Structs (Value(..), Operator(..), Instruction(..))

vmTests :: IO ()
vmTests = hspec $ do
    describe "Test the exec function" $ do
        it "executes a simple addition" $ do
            let instructions = [Push (Numerical 2), Push (Numerical 3), Push (Operator Add), Call, Ret]
            let result = exec [] instructions [] [] [] []
            result `shouldBe` Right (Numerical 5)

        it "handles division by zero" $ do
            let instructions = [Push (Numerical 2), Push (Numerical 0), Push (Operator Div), Call, Ret]
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

            let instructions = [Push (Numerical 5), Push (Function factFunction), Call, Ret]
            let result = exec [] instructions [] [] [] []
            result `shouldBe` Right (Numerical 120)
        it "executes a function call from environment" $ do
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
                     Push (Numerical 2),
                     PushEnv "fun",
                     Call,
                     Push (Numerical 1),
                     Push (Operator Add),
                     Call,
                     Ret]
            let result = exec [] instructions [] env [] []
            result `shouldBe` Right (Numerical 13)
