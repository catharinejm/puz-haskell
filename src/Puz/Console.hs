module Puz.Console where

import Puz.Prelude
import Puz.Printer
import Puz.Types
import Puz.Util
import System.IO

prompt :: (MonadIO m) => m ()
prompt = liftIO $ do
  putStr "> "
  hFlush stdout

console :: (Game m) => m ()
console = do
  prompt
  cmd <- liftIO getLine >>= return . map toLower
  dispatch cmd
  where
    dispatch :: (Game m) => String -> m ()
    dispatch "quit" = setMode Quit
    dispatch "exit" = setMode Quit
    dispatch "help" = do
      liftIO . putStrLn . unlines $ [ "Commands:"
                                    , "  board - Display the board"
                                    , "  solution - Display the solution"
                                    , "  clues - Display the clues"
                                    , "  play - Play the crossword!"
                                    ]
    dispatch "board" = printPlayerBoard
    dispatch "solution" = printSolution
    dispatch "clues" = do
      clues <- asks (clues :: Puzzle -> [Clue])
      liftIO . putStrLn . unlines $ clueStrs clues
      where
        clueStrs = map cs . sortBy (compare `on` ((,) <$> direction <*> number))
        cs Clue{..} = show number ++ " " ++ show direction ++ ": " ++ text
    dispatch "play" = setMode Play
    dispatch s = liftIO (putStrLn $ s ++ "?? Wut?")
