import Data.Binary
import GHC.Generics
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

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
