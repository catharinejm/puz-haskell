module Puz.Util where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Puz.Prelude

byteSlice :: Int -> Int -> ByteString -> ByteString
byteSlice start len = BS.take len . BS.drop start

