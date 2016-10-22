module Puz
    ( module Puz.Prelude
    , module Puz
    ) where

import Puz.Errors
import Puz.Prelude
import Puz.Printer
import Puz.Reader
import Puz.Types
import Puz.Validator
import System.IO

loadGame :: (MonadIO m, MonadError PuzError m) => FilePath -> m Puzzle
loadGame puzFile = do
  (puz, bs) <- readPuz puzFile
  ensureUnscrambled puz
  runChecksums puz bs
  mkPuzzle puz

runGame :: (MonadIO m, MonadError PuzError m, MonadState Puzzle m) => m ()
runGame = do
  liftIO $ do
    putStrLn "Puzzle loaded!"
    putStrLn "Type 'help' for more info."
    putStrLn ""
  repl
  where
    repl = do
      liftIO $ do
        putStr "> "
        hFlush stdout
      cmd <- liftIO getLine
      if cmd `elem` ["quit", "exit"]
        then return ()
        else dispatch cmd >> repl
    dispatch "help" =
      liftIO . putStrLn . unlines $ [ "Commands:"
                                    , "  board - Display the board"
                                    , "  solution - Display the solution"
                                    , "  clues - Display the clues"
                                    ]
    dispatch "board" =
      gets (board :: Puzzle -> Board) >>= liftIO . putStrLn . printBoard
    dispatch "solution" =
      gets (solution :: Puzzle -> Board) >>= liftIO . putStrLn . printBoard
    dispatch s = liftIO . putStrLn $ s ++ "?? Wut?"

playGame :: (MonadIO m) => FilePath -> m ()
playGame puzFile = do
  res <- runExceptT (loadGame puzFile >>= runStateT runGame)
  case res of
   Left err -> liftIO $ print err
   Right _ -> liftIO $ putStrLn "Good bye!"
