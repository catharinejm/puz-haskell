module Puz
    ( module Puz.Prelude
    , module Puz
    ) where

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as BSL
import           Puz.Prelude
import           Puz.Types
import           System.IO

someFunc :: IO ()
someFunc = do
  puzFile <- openFile "resources/avxword20151231.puz" ReadMode
  rawPuz <- BSL.hGetContents puzFile
  let puz = B.runGet B.get rawPuz :: PuzBinary
  print puz
