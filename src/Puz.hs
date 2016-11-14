module Puz
    ( module Puz.Prelude
    , module Puz
    ) where

import Puz.Console
import Puz.Play
import Puz.Prelude
import Puz.Reader
import Puz.Types
import Puz.Util
import Puz.Validator

loadGame :: (MonadIO m, MonadError PuzError m) => FilePath -> m (Puzzle, GameState)
loadGame puzFile = do
  (puz, bs) <- readPuz puzFile
  ensureUnscrambled puz
  runChecksums puz bs
  mkPuzzle puz

runGame :: (Game m) => m ()
runGame = do
  liftIO $ do
    putStrLn "Puzzle loaded!"
    putStrLn ""
  printNotNull (pf title)
  printNotNull (pf author)
  printNotNull (pf copyright)
  printNotNull (pf notes)
  liftIO $ do
    putStrLn "Type 'help' for more info."
    putStrLn ""
  loop
  where
    loop = do
      mode <- gets currentMode
      case mode of
       Console -> console >> loop
       Play -> play >> loop
       Quit -> return ()

playGame :: (MonadIO m) => FilePath -> m ()
playGame puzFile = do
  res <- runExceptT (loadGame puzFile >>= \(p, s) -> runRWST runGame p s)
  case res of
   Left err -> liftIO $ print err
   Right _ -> shutdown

shutdown :: (MonadIO m) => m ()
shutdown = liftIO (putStrLn "Good bye!" >> exitSuccess)
