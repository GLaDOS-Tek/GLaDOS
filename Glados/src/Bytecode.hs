import GHC.Generics
import Data.Binary
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)

data Value =
    Numerical Int
    | Boolean Bool
    | Operator Operators
    | Insts [Instruction]
    | Function Insts
    deriving (Generic)
instance Show Value

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
    deriving (Generic)
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

type Insts = [Instruction]
data Instruction =
    Push Value
    | Pop
    | Call
    | Ret
    | JumpIfFalse Int
    | PushArg Int
    | PushEnv String
    deriving (Generic)
instance Show Instruction where
    show (Push x) = "Push " ++ show x
    show (Pop) = "Pop"
    show (Call) = "Call"
    show (Ret) = "Ret"

instance Binary Value
instance Binary Operators
instance Binary Instruction

type Env = [(String, Value)]

encodeToBinary :: Env -> ByteString
encodeToBinary = encode

binaryToDecode :: ByteString -> Env
binaryToDecode = decode

writeBinaryToFile :: FilePath -> Env -> IO ()
writeBinaryToFile filename dataToWrite = do
    let encodedData = encodeToBinary dataToWrite
    B.writeFile filename encodedData

readBinaryFromFile :: FilePath -> IO Env
readBinaryFromFile filename = do
    encodedData <- B.readFile filename
    return (binaryToDecode encodedData)
