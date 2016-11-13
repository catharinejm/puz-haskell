module Puz.Reader where

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
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

mkPuzzle :: (MonadError PuzError m) => PuzResult -> m (Puzzle, GameState)
mkPuzzle PuzResult{..} = do
  sln <- brd solution
  bd <- brd board
  clues <- mkClues clues bd
  let puz = Puzzle { title = fromCString title
                   , author = fromCString author
                   , copyright = fromCString copyright
                   , notes = fromCString notes
                   , solution = sln
                   , clues = clues
                   , cluesByNum = M.map head $ toMapBy (\Clue{..} -> (number, direction)) clues
                   , cluesByCoord = toMapBy coords clues
                   , messageRow = (fromIntegral height * 2 + 2)
                   , clueRow = (fromIntegral height * 2 + 3)
                   }
      gameState = mkGameState bd
  return (puz, gameState)
  where
    brd = mkBoardM (fromIntegral width) (fromIntegral height)

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

data ClueBuilder = CB { clues :: ![Clue]
                      , clueNum :: !Int
                      , clueTexts :: ![String]
                      }

newCB :: [String] -> ClueBuilder
newCB = CB [] 1

mkClues :: (MonadError PuzError m) => [ByteString] -> Board -> m [Clue]
mkClues texts board@Board{..} = let CB{..} = buildClues
                                in return clues
  where
    needsAcrossNum x y = isWhite (x, y) board && isBlack (x-1, y) board && isWhite (x+1, y) board
    needsDownNum x y = isWhite (x, y) board && isBlack (x, y-1) board && isWhite (x, y+1) board
    buildClues = flip execState (newCB $ map fromCString texts) $ do
      mapM_ buildClue [ (x, y) | y <- [0..height-1], x <- [0..width-1] ]
    buildClue coords@(x, y) = do
      let needsAx = needsAcrossNum x y
          needsDn = needsDownNum x y
      when needsAx addAx
      when needsDn addDn
      when (needsAx || needsDn) increment
      where
        setClue dir cb@CB{clueTexts=(c:cs), ..} =
          let clue = Clue clueNum dir coords c
          in cb { clues = clue : clues
                , clueTexts = cs
                }
        addAx = modify (setClue Across)
        addDn = modify (setClue Down)
        increment = modify (\cb@CB{clueNum} -> cb { clueNum = clueNum + 1 })
      
