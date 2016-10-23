module Puz.Util where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Vector ((!?))
import           Puz.Prelude
import           Puz.Types


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
