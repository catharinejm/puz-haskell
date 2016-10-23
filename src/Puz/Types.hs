module Puz.Types where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Data.Vector (Vector)
import           Data.Map.Strict (Map)
import           Puz.Prelude hiding (get, put)

data PuzResult = PuzResult { checksum :: !Word16
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
                           , solution :: !ByteString
                           , board :: !ByteString
                           , title :: !ByteString
                           , author :: !ByteString
                           , copyright :: !ByteString
                           , clues :: ![ByteString]
                           , notes :: !ByteString
                           , extraSections :: ![ByteString]
                           }
               deriving (Show)

instance Binary PuzResult where
  get = do
    checksum <- getWord16le
    magic <- getByteString 12
    cibChecksum <- getWord16le
    maskedLowChecksums <- getWord32le
    maskedHighChecksums <- getWord32le
    versionString <- getByteString 4
    reserved0x1C <- getWord16le
    scrambledChecksum <- getWord16le
    reserved0x20 <- getByteString 12
    width <- getWord8
    height <- getWord8
    numClues <- getWord16le
    unknownBitmask <- getWord16le
    scrambledTag <- getWord16le
    solution <- getByteString (fromIntegral width * fromIntegral height)
    board <- getByteString (fromIntegral width * fromIntegral height)
    title <- getString
    author <- getString
    copyright <- getString
    clueTexts <- replicateM (fromIntegral numClues) (getString >>= return . BS.init)
    notes <- getString
    extraSections <- getExtraSections []
    return $ PuzResult { checksum = checksum
                       , magic = magic
                       , cibChecksum = cibChecksum
                       , maskedLowChecksums = maskedLowChecksums
                       , maskedHighChecksums = maskedHighChecksums
                       , versionString = versionString
                       , reserved0x1C = reserved0x1C
                       , scrambledChecksum = scrambledChecksum
                       , reserved0x20 = reserved0x20
                       , width = width
                       , height = height
                       , numClues = numClues
                       , unknownBitmask = unknownBitmask
                       , scrambledTag = scrambledTag
                       , solution = solution
                       , board = board
                       , title = title
                       , author = author
                       , copyright = copyright
                       , clues = clueTexts
                       , notes = notes
                       , extraSections = extraSections
                       }
    where
      getString = getString' []
      getString' acc = do
        b <- getWord8
        if b == 0
          then return . BS.pack . reverse $ b:acc
          else getString' (b:acc)
      getExtraSections acc = do
        empty <- isEmpty
        if empty
          then return $ reverse acc
          else getString >>= getExtraSections . (: acc)

  put PuzResult{..} = do
    putWord16le checksum
    putByteString magic
    putWord16le cibChecksum
    putWord32le maskedLowChecksums
    putWord32le maskedHighChecksums
    putByteString versionString
    putWord16le reserved0x1C
    putWord16le scrambledChecksum
    putByteString reserved0x20
    putWord8 width
    putWord8 height
    putWord16le numClues
    putWord16le unknownBitmask
    putWord16le scrambledTag
    putByteString solution
    putByteString board
    put title
    put author
    put copyright
    mapM_ put clues
    where
      putClues clues = mapM_ (put . text) clues

type CluesByNum = Map (Int, Direction) Clue
type CluesByCoord = Map (Int, Int) [Clue]

data Puzzle = Puzzle { title :: !String
                     , author :: !String
                     , copyright :: !String
                     , notes :: !String
                     , solution :: !Board
                     , clues :: ![Clue]
                     , cluesByNum :: !CluesByNum
                     , cluesByCoord :: !CluesByCoord
                     }
            deriving (Show)

data Clue = Clue { number :: !Int
                 , direction :: !Direction
                 , coords :: !(Int, Int)
                 , text :: !String
                 }
          deriving (Show)

data Direction = Across | Down deriving (Show, Eq, Ord)

data Board = Board { width :: !Int
                   , height :: !Int
                   , rows :: !(Vector (Vector Cell))
                   }
           deriving (Show)

data Cell = Blocked
          | Empty
          | Filled !Char
          deriving (Show, Eq)

data GameState = GameState { currentBoard :: !Board
                           , playerPosition :: !(Int, Int)
                           , playerDirection :: Direction
                           }
               deriving (Show)

newtype PuzError = PuzError String

instance Show PuzError where
  show (PuzError err) = "ERROR: " ++ err

type Game m = ( MonadError PuzError m
              , MonadState GameState m
              , MonadReader Puzzle m
              , MonadWriter () m
              , MonadIO m
              )
