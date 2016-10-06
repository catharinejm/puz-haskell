module Puz.Types where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Puz.Prelude

data PuzBinary = PuzBinary { checksum :: !Word16
                           , magic :: !ByteString -- 12 Bytes
                           , cibChecksum :: !Word16
                           , maskedLowChecksums :: !Word32
                           , maskedHighChecksums :: !Word32
                           , versionString :: !ByteString
                           , reserved0x1C :: !Word16
                           , scrambledChecksum :: !Word16
                           , reserved0x20 :: !ByteString -- 12 Bytes
                           , width :: !Word8
                           , height :: !Word8
                           , numClues :: !Word16
                           , unknownBitmask :: !Word16
                           , scrambledTag :: !Word16
                           , puzzleSolution :: !Board
                           , board :: !Board
                           , clues :: ![Clue]
                             -- Extra stuff, coming later...
                           }

data Clue = Clue { number :: !Int, text :: !String }

data Board = Board { width :: !Int
                   , height :: !Int
                   , rows :: !(Vector (Vector Cell))
                   }

data Cell = Blocked
          | Empty
          | Filled !Char
