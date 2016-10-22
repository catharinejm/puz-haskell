module Puz.Util where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Puz.Prelude


byteSlice :: Int -> Int -> ByteString -> ByteString
byteSlice start len = BS.take len . BS.drop start

bytePartition :: Int -> ByteString -> [ByteString]
bytePartition len bs = mkParts [] bs
  where
    mkParts acc bs
      | BS.null bs = reverse acc
      | otherwise = mkParts (BS.take len bs : acc) $ BS.drop len bs
