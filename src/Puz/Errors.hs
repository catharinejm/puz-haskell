module Puz.Errors where

import Puz.Prelude
 
newtype PuzError = PuzError String

instance Show PuzError where
  show (PuzError err) = "ERROR: " ++ show err

cibChecksumError :: Word16 -> Word16 -> PuzError
cibChecksumError expected actual =
  PuzError $ "Invalid CIB Checksum. Expected " ++ show expected ++ ", Got: " ++ show actual
