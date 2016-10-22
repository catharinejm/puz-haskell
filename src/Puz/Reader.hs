module Puz.Reader where

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import           Puz.Errors
import           Puz.Prelude
import           Puz.Types
import           Puz.Util
import           Puz.Validator
import           System.IO

readPuz :: (MonadIO m, MonadError PuzError m) => FilePath -> m (PuzResult, ByteString)
readPuz fileName = do
  puzHandle <- liftIO $ openFile fileName ReadMode
  rawPuz <- liftIO $ BSL.hGetContents puzHandle
  validateHeader rawPuz
  let puz = B.runGetOrFail (B.get :: B.Get PuzResult) rawPuz
  case puz of
   Left (_, _, err) -> throwError $ PuzError err
   Right (rem, _, puz)
     | BSL.null rem -> return (puz, BSL.toStrict rawPuz)
     | otherwise -> throwError $ PuzError "Contents remain after parsing file!"

mkPuzzle :: (MonadError PuzError m) => PuzResult -> m Puzzle
mkPuzzle PuzResult{..} = do
  sln <- brd solution
  bd <- brd board
  return $ Puzzle { title = str title
                  , author = str author
                  , copyright = str copyright
                  , notes = str notes
                  , solution = sln
                  , board = bd
                  , clues = buildClues
                  }
  where
    str = CS.unpack . BS.takeWhile (/= 0)
    brd = mkBoardM (fromIntegral width) (fromIntegral height)
    buildClues = []


mkBoard :: Int -> Int -> ByteString -> Maybe Board
mkBoard width height bytes =
  if correctHeight && correctWidths
  then Just $ Board width height rows
  else Nothing
  where
    byteRows = bytePartition width bytes
    rows = V.fromList $ map (V.fromList . map mkCell . CS.unpack) byteRows
    correctHeight = length byteRows == height
    correctWidths = all ((== width) . BS.length) byteRows

mkBoardM :: (MonadError PuzError m) => Int -> Int -> ByteString -> m Board
mkBoardM width height bytes = maybe err return (mkBoard width height bytes)
  where
    err = throwError $ PuzError "Unable to parse board!"

mkCell :: Char -> Cell
mkCell '.' = Blocked
mkCell '-' = Empty
mkCell c = Filled c

unCell :: Cell -> Char
unCell Blocked = '.'
unCell Empty = '-'
unCell (Filled c) = c
