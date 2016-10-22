module Puz
    ( module Puz.Prelude
    , module Puz
    ) where

import qualified Data.ByteString as BS
import           Puz.Prelude
import           Puz.Reader
import           Puz.Types

playGame :: (MonadIO m) => FilePath -> m ()
playGame puzFile = do
  (puz @ PuzResult{..}, bs) <- readPuz puzFile
  res <- runExceptT (runChecksums puz bs)
  case res of
   Left err -> liftIO $ print err
   Right _ -> liftIO $ putStrLn "Checksums passed!"
