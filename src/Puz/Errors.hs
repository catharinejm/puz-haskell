module Puz.Errors where

import Puz.Prelude
 
newtype PuzError = PuzError String

instance Show PuzError where
  show (PuzError err) = "ERROR: " ++ show err

expectedActual :: (Show a, Show b) => a -> b -> String
expectedActual expected actual = "Expected: " ++ show expected ++ ", Got: " ++ show actual

cibChecksumError :: Word16 -> Word16 -> PuzError
cibChecksumError expected actual =
  PuzError $ "Invalid CIB Checksum. " ++ expectedActual expected actual

primaryChecksumError :: Word16 -> Word16 -> PuzError
primaryChecksumError expected actual =
  PuzError $ "Invalid Primary Checksum. " ++ expectedActual expected actual

maskedLowChecksumError :: Word32 -> Word32 -> PuzError
maskedLowChecksumError expected actual =
  PuzError $ "Invalid Masked Low Checksum. " ++ expectedActual expected actual

maskedHighChecksumError :: Word32 -> Word32 -> PuzError
maskedHighChecksumError expected actual =
  PuzError $ "Invalid Masked High Checksum. " ++ expectedActual expected actual
