module Puz.Util where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Vector ((!?), (!))
import qualified Data.Vector as V
import           Puz.Prelude
import           Puz.Types

(//?) :: V.Vector a -> [(Int, a)] -> V.Vector a
(//?) = V.unsafeUpd

byteSlice :: Int -> Int -> ByteString -> ByteString
byteSlice start len = BS.take len . BS.drop start

bytePartition :: Int -> ByteString -> [ByteString]
bytePartition len bs = mkParts [] bs
  where
    mkParts acc bs
      | BS.null bs = reverse acc
      | otherwise = mkParts (BS.take len bs : acc) $ BS.drop len bs

fromCString :: ByteString -> String
fromCString = CS.unpack . BS.takeWhile (/= 0)

toMapBy :: (Ord b) => (a -> b) -> [a] -> Map b [a]
toMapBy f as = foldl' (\acc a -> M.alter (maybe (Just [a]) (Just . (a:))) (f a) acc) M.empty as

printNotNull :: (MonadReader r m, MonadIO m) => (r -> String) -> m ()
printNotNull = (uncurry when . (not . null &&& liftIO . putStrLn) =<<) . asks

pf :: (Puzzle -> a) -> (Puzzle -> a)
pf = id

getCell :: (Int, Int) -> Board -> Maybe Cell
getCell (x, y) Board{rows} = rows !? y >>= (!? x)

getCellM :: (MonadState GameState m) => (Int, Int) -> m (Maybe Cell)
getCellM coords = get >>= return . getCell coords . currentBoard

getCurrentCell :: (MonadState GameState m) => m (Maybe Cell)
getCurrentCell = gets playerPosition >>= getCellM

getPreviousCell :: (MonadState GameState m) => m (Maybe Cell)
getPreviousCell = do
  GameState{playerPosition=(x, y), ..} <- get
  case playerDirection of
   Across -> getCellM (x-1, y)
   Down -> getCellM (x, y-1)

backVec :: Direction -> (Int, Int)
backVec Across = (-1, 0)
backVec Down = (0, -1)

getPreviousCoords :: (MonadState GameState m) => m (Int, Int)
getPreviousCoords = do
  GameState{..} <- get
  return $ findCoords playerPosition (backVec playerDirection) currentBoard
  where
    sumCoords (x, y) (x', y') = (x+x', y+y')
    findCoords pos vec board =
      case getCell (sumCoords pos vec) board of
       Nothing -> pos
       Just Blocked -> findCoords pos (sumCoords vec vec) board
       Just _ -> sumCoords pos vec

setCell :: (Int, Int) -> Cell -> Board -> Board
setCell (x, y) cell board@Board{rows} =
  board { rows = rows //? [(y, (rows ! y) //? [(x, cell)])] }

setCellM :: (MonadState GameState m) => (Int, Int) -> Cell -> m ()
setCellM coords cell = do
  modify $ \s@GameState{currentBoard} -> s { currentBoard = setCell coords cell currentBoard }
