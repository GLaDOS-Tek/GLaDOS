
module Bytecode (
    writeBinaryToFile,
    readBinaryFromFile
) where

import Structs
import GHC.Generics
import Data.Binary
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

type SaveData = (EnvVM, Insts)

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
