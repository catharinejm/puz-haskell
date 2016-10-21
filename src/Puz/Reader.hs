module Puz.Reader where

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Puz.Prelude
import           Puz.Types
import           System.IO

readPuz :: FilePath -> IO (PuzResult, ByteString)
readPuz fileName = do
  puzHandle <- openFile fileName ReadMode
  rawPuz <- BSL.hGetContents puzHandle
  let puz = B.runGet B.get rawPuz :: PuzResult
  return (puz, BSL.toStrict rawPuz)

checksumRegion :: ByteString -> Word16 -> Word16
checksumRegion bytes ckInit = BS.foldl doChecksum ckInit bytes
  where
    doChecksum cksum byte = if cksum .&. 0x0001 /= 0
                            then cksum `shiftR` 1 + 0x8000
                            else cksum `shiftR` 1 + fromIntegral byte

byteSlice :: Int -> Int -> ByteString -> ByteString
byteSlice start len = BS.take len . BS.drop start

runChecksum :: PuzResult -> ByteString -> Bool
runChecksum PuzResult{..} bytes = cibChecksum == checksumRegion (byteSlice 0x2C 8 bytes) 0
  
