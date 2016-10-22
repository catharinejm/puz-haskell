{-# LANGUAGE FlexibleContexts #-}

module Puz.Reader where

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Puz.Errors
import           Puz.Prelude
import           Puz.Types
import           System.IO

readPuz :: (MonadIO m) => FilePath -> m (PuzResult, ByteString)
readPuz fileName = do
  puzHandle <- liftIO $ openFile fileName ReadMode
  rawPuz <- liftIO $ BSL.hGetContents puzHandle
  let puz = B.runGet B.get rawPuz :: PuzResult
  return (puz, BSL.toStrict rawPuz)

checksumRegion :: ByteString -> Word16 -> Word16
checksumRegion bytes ckInit = BS.foldl doChecksum ckInit bytes
  where
    doChecksum cksum byte = let ck' = if odd cksum
                                      then cksum `rotateR` 1
                                      else cksum `shiftR` 1
                            in ck' + fromIntegral byte

byteSlice :: Int -> Int -> ByteString -> ByteString
byteSlice start len = BS.take len . BS.drop start

runChecksums :: (MonadError PuzError m) => PuzResult -> ByteString -> m ()
runChecksums PuzResult{..} bytes = do
  validateCIB
  where
    validateCIB = do
      let ck = checksumRegion (byteSlice 0x2C 8 bytes) 0
      when (cibChecksum /= ck) $ throwError (cibChecksumError cibChecksum ck)
