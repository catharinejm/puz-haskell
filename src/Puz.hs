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
      dir <- gets playerDirection
      liftIO (clearLine >> (putStrLn . map toUpper $ show dir))
      c <- liftIO getChar
      dispatch c
      -- liftIO $ hFlush stdout
      loop
    dispatch (ctrl -> 'n') = moveDown
    dispatch (ctrl -> 'p') = moveUp
    dispatch (ctrl -> 'b') = moveLeft
    dispatch (ctrl -> 'f') = moveRight
    dispatch (ctrl -> 'd') = liftIO clearLine >> shutdown
    dispatch ' ' = toggleDirection
    dispatch c@(isLetter -> True) = fillCurrentCell (Just c)
    dispatch '\DEL' = fillCurrentCell Nothing
    dispatch '\ESC' = do
      c <- liftIO getChar
      case c of
       '[' -> do
         c' <- liftIO getChar
         case c' of
          'A' -> moveUp
          'B' -> moveDown
          'C' -> moveRight
          'D' -> moveLeft
          d@(isDigit -> True) -> getNum [d] >>= \n ->
            case n of
             "3" -> get >>= \GameState{..} -> setCellM playerPosition Empty
             _ -> return ()
          _ -> return ()
       _ -> return ()
    dispatch _ = liftIO $ putStrLn "Nothing to do..."
    ctrl c = chr (ord c + ord 'a' - 1)
    getNum digits = liftIO getChar >>= \c -> case c of
                                              '~' -> return (reverse digits)
                                              n -> getNum (n:digits)
    toggleDirection = modify $ \s ->
      s { playerDirection = case (playerDirection s) of
                             Across -> Down
                             Down -> Across
        }

move :: (Game m) => (Int, Int) -> m ()
move (dx, dy) = do
  (x, y) <- gets playerPosition
  mcell <- getCellM (x + dx, y + dy)
  case mcell of
   Nothing -> return ()
   Just Blocked -> move (dx + signum dx, dy + signum dy)
   Just _ -> modify $ \gs -> gs { playerPosition = (x + dx, y + dy) }

moveDown, moveUp, moveLeft, moveRight :: (Game m) => m ()
moveDown = printMessage "MOVE DOWN" >> move (0, 1)
moveUp = printMessage "MOVE UP" >> move (0, -1)
moveLeft = printMessage "MOVE LEFT" >> move (-1, 0)
moveRight = printMessage "MOVE RIGHT" >> move (1, 0)

backwardVector :: (Game m) => m (Int, Int)
backwardVector = do
  GameState{..} <- get
  case playerDirection of
   Across -> return (-1, 0)
   Down -> return (0, -1)

forwardVector :: (Game m) => m (Int, Int)
forwardVector = do
  GameState{..} <- get
  case playerDirection of
   Across -> return (1, 0)
   Down -> return (0, 1)

moveBack :: (Game m) => m ()
moveBack = backwardVector >>= move

moveForward :: (Game m) => m ()
moveForward = forwardVector >>= move

fillCurrentCell :: (Game m) => Maybe Char -> m ()
fillCurrentCell mchr = do
  GameState{..} <- get
  Just curCell <- getCurrentCell
  case mchr of
   Nothing -> case curCell of
               Empty -> getPreviousCoords >>= (`setCellM` Empty) >> moveBack
               Filled _ -> setCellM playerPosition Empty
   Just c -> setCellM playerPosition (Filled $ toUpper c) >> moveForward

shutdown :: (MonadIO m) => m ()
shutdown = liftIO (putStrLn "Good bye!" >> exitSuccess)

playGame :: (MonadIO m) => FilePath -> m ()
playGame puzFile = do
  res <- runExceptT (loadGame puzFile >>= \(p, s) -> runRWST runGame p s)
  case res of
   Left err -> liftIO $ print err
   Right _ -> shutdown
