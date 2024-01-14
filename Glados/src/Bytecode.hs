{-# LANGUAGE DeriveGeneric #-}

module Bytecode (
    writeBinaryToFile,
    readBinaryFromFile
) where

import GHC.Generics
import Data.Binary
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

data Value =
      Numerical Int
    | Boolean Bool
    | Operator Operators
    | Insts [Instruction]
    | Function Insts
    deriving (Show, Generic)

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
    deriving (Show, Generic)

type Insts = [Instruction]

type SaveData = (Env, Insts)

data Instruction =
      Push Value
    | Pop
    | Call
    | Ret
    | JumpIfFalse Int
    | PushArg Int
    | PushEnv String
    deriving (Show, Generic)

instance Binary Value
instance Binary Operators
instance Binary Instruction

type Env = [(String, Value)]

encodeToBinary :: SaveData -> ByteString
encodeToBinary = encode

binaryToDecode :: ByteString -> SaveData
binaryToDecode = decode

writeBinaryToFile :: FilePath -> SaveData -> IO ()
writeBinaryToFile filename dataToWrite = do
    let encodedData = encodeToBinary dataToWrite
    B.writeFile filename encodedData

readBinaryFromFile :: FilePath -> IO SaveData
readBinaryFromFile filename = do
    encodedData <- B.readFile filename
    return (binaryToDecode encodedData)
