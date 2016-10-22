module Puz.Validator where

import qualified Data.Binary.Get as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CS
import qualified Data.ByteString.Lazy as BSL
import           Puz.Errors
import           Puz.Prelude
import           Puz.Types
import           Puz.Util

validateHeader :: (MonadError PuzError m) => BSL.ByteString -> m ()
validateHeader bytes = do
  let magicRes = B.runGetOrFail (B.skip 2 >> B.getByteString 12) bytes
  case magicRes of
   Left (_, _, err) -> throwError $ PuzError err
   Right (_, _, magic) -> do
     if magic == CS.pack "ACROSS&DOWN\NUL"
       then return ()
       else throwError $ PuzError $ "Invalid magic: " ++ show magic

checksumRegion :: Word16 -> ByteString -> Word16
checksumRegion = BS.foldl doChecksum
  where
    doChecksum cksum byte = let ck' = if odd cksum
                                      then cksum `rotateR` 1
                                      else cksum `shiftR` 1
                            in ck' + fromIntegral byte

runChecksums :: (MonadError PuzError m) => PuzResult -> ByteString -> m ()
runChecksums PuzResult{..} bytes = do
  validateCIB
  validatePrimary
  validateMasks
  where
    validateCIB = do
      let ck = checksumRegion 0 (byteSlice 0x2C 8 bytes)
      when (cibChecksum /= ck) $ throwError (cibChecksumError cibChecksum ck)
    nonEmptyStrings = filter (not . nullStr) $ [title, author, copyright] ++ clues ++ [notes]
    validatePrimary = do 
      let regions = solution : board : nonEmptyStrings
          ck = checksumFold cibChecksum regions
      when (checksum /= ck) $ throwError (primaryChecksumError checksum ck)
    nullStr s = BS.null s || s == BS.pack [0]
    validateMasks = do
      let solCk = checksumRegion 0 solution
          boardCk = checksumRegion 0 board
          partialCk = checksumFold 0 nonEmptyStrings
          lo = [ 0x49 `xor` cibChecksum
               , 0x43 `xor` solCk
               , 0x48 `xor` boardCk
               , 0x45 `xor` partialCk
               ]
          hi = [ 0x41 `xor` (cibChecksum `shiftR` 8)
               , 0x54 `xor` (solCk `shiftR` 8)
               , 0x45 `xor` (boardCk `shiftR` 8)
               , 0x44 `xor` (partialCk `shiftR` 8)
               ]
      when (word32 lo /= maskedLowChecksums) $
        throwError (maskedLowChecksumError (word32 lo) maskedLowChecksums)
      when (word32 hi /= maskedHighChecksums) $
        throwError (maskedHighChecksumError (word32 lo) maskedHighChecksums)
    checksumFold = foldl' checksumRegion
    word32 = B.runGet B.getWord32le . BSL.pack . map fromIntegral
