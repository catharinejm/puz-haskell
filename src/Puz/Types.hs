module Puz.Types where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Puz.Prelude hiding (get, put)

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
               deriving (Show)

instance Binary PuzBinary where
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
    puzzleSolution <- getBoard (fromIntegral width) (fromIntegral height)
    board <- getBoard (fromIntegral width) (fromIntegral height)
    (title : author : copyright : clueTexts) <- getClues []
    let clues = map (\(t, i) -> Clue i t) (clueTexts `zip` [1..])
    return $ PuzBinary { checksum = checksum
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
                       , puzzleSolution = puzzleSolution
                       , board = board
                       , clues = clues
                       }
    where
      getClues :: [ClueText] -> Get [ClueText]
      getClues acc = do
        empty <- isEmpty
        if empty
          then return (reverse acc)
          else do clueText @ (ClueText txt) <- get
                  if take 4 txt `elem` extraHeadings
                    then return (reverse acc)
                    else getClues (clueText : acc)
      extraHeadings = ["GRBS", "RTBL", "LTIM", "GEXT", "RUSR"]

  put PuzBinary{..} = do
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
    putBoard puzzleSolution
    putBoard board
    putClues clues
    where
      putClues clues = mapM_ (put . text) clues

data Clue = Clue { number :: !Int, text :: !ClueText }
          deriving (Show)

newtype ClueText = ClueText { unClueText :: String }
                 deriving (Show)

instance Binary ClueText where
  get = doGet [] >>= return . ClueText
    where
      doGet s = do
        empty <- isEmpty
        if empty
          then finish
          else do c <- getWord8
                  if c == 0
                    then finish
                    else doGet (c : s)
        where
          finish = return $ map (chr . fromIntegral) (reverse s)

  put (ClueText txt) = putByteString (BS.pack txt) >> putWord8 0
        
data Board = Board { width :: !Int
                   , height :: !Int
                   , rows :: !(Vector (Vector Cell))
                   }
           deriving (Show)

getBoard :: Int -> Int -> Get Board
getBoard width height = do
  rowTexts <- replicateM height (getByteString width)
  let rows = V.fromList $ map (V.fromList . map mkCell . BS.unpack) rowTexts
  return $ Board width height rows

putBoard :: Board -> Put
putBoard Board{..} = do
  mapM_ putRow rows
  where
    putRow = mapM_ (putWord8 . fromIntegral . ord . unCell)

data Cell = Blocked
          | Empty
          | Filled !Char
          deriving (Show)

mkCell :: Char -> Cell
mkCell '.' = Blocked
mkCell '-' = Empty
mkCell c = Filled c

unCell :: Cell -> Char
unCell Blocked = '.'
unCell Empty = '-'
unCell (Filled c) = c
