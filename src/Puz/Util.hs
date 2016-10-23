module Puz.Util where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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

printNotNull :: (MonadState s m, MonadIO m) => (s -> String) -> m ()
printNotNull = (uncurry when . (not . null &&& liftIO . putStrLn) =<<) . gets

pf :: (Puzzle -> a) -> (Puzzle -> a)
pf = id

getPuz :: (MonadState Puzzle m) => (Puzzle -> a) -> m a
getPuz = gets

getBoard :: (MonadState Puzzle m) => (Board -> a) -> m a
getBoard f = getPuz board >>= return . f
