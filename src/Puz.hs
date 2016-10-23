{-# LANGUAGE ScopedTypeVariables #-}

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
import Puz.Util
import System.IO

loadGame :: (MonadIO m, MonadError PuzError m) => FilePath -> m Puzzle
loadGame puzFile = do
  (puz, bs) <- readPuz puzFile
  ensureUnscrambled puz
  runChecksums puz bs
  mkPuzzle puz

runGame :: forall m. (MonadIO m, MonadError PuzError m, MonadState Puzzle m) => m ()
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
  repl
  where
    pf :: (Puzzle -> a) -> (Puzzle -> a)
    pf = id
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
                                    , "  play - Play the crossword!"
                                    ]
    dispatch "board" =
      gets (board :: Puzzle -> Board) >>= liftIO . putStrLn . printBoard
    dispatch "solution" =
      gets (solution :: Puzzle -> Board) >>= liftIO . putStrLn . printBoard
    dispatch "clues" = do
      clues <- gets (clues :: Puzzle -> [Clue])
      liftIO . putStrLn . unlines $ clueStrs clues
      where
        clueStrs = map cs . sortBy (compare `on` ((,) <$> direction <*> number))
        cs Clue{..} = show number ++ " " ++ show direction ++ ": " ++ text
    dispatch s = liftIO . putStrLn $ s ++ "?? Wut?"

playGame :: (MonadIO m) => FilePath -> m ()
playGame puzFile = do
  res <- runExceptT (loadGame puzFile >>= runStateT runGame)
  case res of
   Left err -> liftIO $ print err
   Right _ -> liftIO $ putStrLn "Good bye!"
