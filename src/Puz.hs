{-# LANGUAGE ScopedTypeVariables #-}

module Puz
    ( module Puz.Prelude
    , module Puz
    ) where

import Puz.Prelude
import Puz.Printer
import Puz.Reader
import Puz.Types
import Puz.Util
import Puz.Validator
import System.Console.ANSI
import System.IO

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
    dispatch "play" = startPlaying
    dispatch s = liftIO . putStrLn $ s ++ "?? Wut?"

startPlaying :: (Game m) => m ()
startPlaying = do
  liftIO $ do
    hSetBuffering stdin NoBuffering
    hSetEcho stdout False
    liftIO clearScreen
  loop
  where
    loop = do
      resetScreen
      printPlayerBoard
      c <- liftIO $ getChar
      dispatch c
      -- liftIO $ hFlush stdout
      loop
    dispatch 'j' = liftIO (clearLine >> putStrLn "MOVE DOWN") >> moveDown
    dispatch 'k' = liftIO (clearLine >> putStrLn "MOVE UP") >> moveUp
    dispatch 'h' = liftIO (clearLine >> putStrLn "MOVE LEFT") >> moveLeft
    dispatch 'l' = liftIO (clearLine >> putStrLn "MOVE RIGHT") >> moveRight
    dispatch _ = liftIO $ putStrLn "Nothing to do..."
    move (dx, dy) = do
      (x, y) <- gets playerPosition
      mcell <- getCellM (x + dx, y + dy)
      case mcell of
       Nothing -> return ()
       Just Blocked -> move (dx + signum dx, dy + signum dy)
       Just _ -> modify $ \gs -> gs { playerPosition = (x + dx, y + dy) }
    moveDown = move (0, 1)
    moveUp = move (0, -1)
    moveLeft = move (-1, 0)
    moveRight = move (1, 0)

playGame :: (MonadIO m) => FilePath -> m ()
playGame puzFile = do
  res <- runExceptT (loadGame puzFile >>= \(p, s) -> runRWST runGame p s)
  case res of
   Left err -> liftIO $ print err
   Right _ -> liftIO $ putStrLn "Good bye!"
