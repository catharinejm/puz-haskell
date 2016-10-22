module Puz
    ( module Puz.Prelude
    , module Puz
    ) where

import Puz.Errors
import Puz.Prelude
import Puz.Reader
import Puz.Types
import Puz.Validator

loadGame :: (MonadIO m, MonadError PuzError m) => FilePath -> m PuzResult
loadGame puzFile = readPuz puzFile >>= \(puz, bs) -> runChecksums puz bs >> return puz

playGame :: (MonadIO m) => FilePath -> m ()
playGame puzFile = do
  puz <- runExceptT (loadGame puzFile)
  case puz of
   Left err -> liftIO $ print err
   Right _ -> liftIO $ putStrLn "Validations passed!"
