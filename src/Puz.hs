module Puz
    ( module Puz.Prelude
    , module Puz
    ) where

import qualified Data.ByteString as BS
import           Puz.Prelude
import           Puz.Reader
import           Puz.Types

playGame :: FilePath -> IO ()
playGame puzFile = do
  (puz @ PuzResult{..}, bs) <- readPuz puzFile
  let slice = byteSlice 0x2C 8 bs
      ck = checksumRegion slice 0
  putStrLn $ "Bytes   : " ++ (show . BS.unpack $ slice)
  putStrLn $ "Expected: " ++ show cibChecksum
  putStrLn $ "Actual  : " ++ show ck
