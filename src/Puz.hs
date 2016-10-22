module Puz
    ( module Puz.Prelude
    , module Puz
    ) where

import Puz.Errors
import Puz.Prelude
import Puz.Reader
import Puz.Types
import Puz.Validator

loadGame :: (MonadIO m, MonadError PuzError m) => FilePath -> m Puzzle
loadGame puzFile = do
  (puz, bs) <- readPuz puzFile
  ensureUnscrambled puz
  runChecksums puz bs
  mkPuzzle puz

playGame :: (MonadIO m) => FilePath -> m ()
playGame puzFile = do
  res <- runExceptT (loadGame puzFile)
  case res of
   Left err -> liftIO $ print err
   Right puz -> liftIO $ do
     putStrLn $ unlines [ "Validations passed!"
                        , ""
                        , show puz
                        ]
