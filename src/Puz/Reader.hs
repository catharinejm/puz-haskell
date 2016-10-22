module Puz.Reader where

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Puz.Errors
import           Puz.Prelude
import           Puz.Types
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
