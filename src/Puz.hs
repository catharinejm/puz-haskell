module Puz
    ( module Puz.Prelude
    , module Puz
    ) where

import qualified Data.Map.Strict as M
import           Puz.Prelude
import           Puz.Printer
import           Puz.Reader
import           Puz.Types
import           Puz.Util
import           Puz.Validator
import           System.Console.ANSI
import           System.IO

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
      cmd <- liftIO getLine >>= return . map toLower
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

startPlaying :: forall m. (Game m) => m ()
startPlaying = do
  modify $ \s -> s { shouldShowRepl = False }
  liftIO $ hSetBuffering stdin NoBuffering
  echoOff
  liftIO clearScreen
  loop
  where
    loop = do
      resetScreen
      printPlayerBoard
      printDirection
      printCurrentClue
      liftIO $ hFlush stdout
      c <- liftIO getChar
      dispatch c
      showRepl <- gets shouldShowRepl
      if showRepl
        then return ()
        else loop
    dispatch (ctrl -> 'a') = beginningOfWord
    dispatch (ctrl -> 'e') = endOfWord
    dispatch (ctrl -> 'n') = moveDown
    dispatch (ctrl -> 'p') = moveUp
    dispatch (ctrl -> 'b') = moveLeft
    dispatch (ctrl -> 'f') = moveRight
    dispatch (ctrl -> 'd') = blankCurrentCell
    dispatch (ctrl -> 'w') = jumpToClue
    dispatch (ctrl -> 'y') = showErrors
    dispatch ' ' = toggleDirection
    dispatch c@(isLetter -> True) = fillCurrentCell (Just c)
    dispatch '\DEL' = fillCurrentCell Nothing
    dispatch '\ESC' = do
      c <- liftIO getChar
      case c of
       'x' -> repl
       '[' -> do
         c' <- liftIO getChar
         case c' of
          'A' -> moveUp
          'B' -> moveDown
          'C' -> moveRight
          'D' -> moveLeft
          d@(isDigit -> True) -> getNum [d] >>= \n ->
            case n of
             "3" -> blankCurrentCell
             _ -> return ()
          _ -> return ()
       _ -> return ()
    dispatch _ = liftIO $ putStrLn "Nothing to do..."
    ctrl c = chr (ord c + ord 'a' - 1)
    getNum digits = liftIO getChar >>= \c -> case c of
                                              '~' -> return (reverse digits)
                                              n -> getNum (n:digits)
    repl = do
      liftIO $ hSetBuffering stdin LineBuffering
      modify $ \s -> s { shouldShowRepl = True }
      echoOn
      liftIO clearScreen
      resetScreen
    toggleDirection = do
      GameState{playerDirection} <- get
      let dir = case playerDirection of
                 Across -> Down
                 Down -> Across
      setDirection dir
    jumpToClue :: m ()
    jumpToClue = do
      showClueNumbers
      printMessage "[A]cross / [D]own ?"
      c <- liftIO getChar >>= return . toLower
      let mdir = case c of
            'a' -> Just Across
            'd' -> Just Down
            _ -> Nothing
      maybe (return ()) (\d -> printMessage (show d ++ " - Enter number: ")) mdir
      mnum <- maybe (return Nothing) (\_ -> echoOn >> liftIO getLine >>= \s -> echoOff >> return (readInt s)) mdir
      mclue <- asks cluesByNum >>= \cs -> return (((,) <$> mnum <*> mdir) >>= (`M.lookup` cs))
      maybe (printMessage "Invalid number") (\c -> printClue c >> setDirection (direction c) >> setPosition (coords c)) mclue

setDirection :: (Game m) => Direction -> m ()
setDirection dir = modify $ \s -> s { playerDirection = dir }

setPosition :: (Game m) => (Int, Int) -> m ()
setPosition pos = modify $ \s -> s { playerPosition = pos }

move :: (Game m) => Bool -> (Int, Int) -> m (Int, Int)
move skipBlocks (dx, dy) = do
  (x, y) <- gets playerPosition
  mcell <- getCellM (x + dx, y + dy)
  case mcell of
   Nothing -> return (x, y)
   Just Blocked -> if skipBlocks
                   then move skipBlocks (dx + signum dx, dy + signum dy)
                   else return (x, y)
   Just _ -> do let npos = (x + dx, y + dy)
                setPosition npos
                return npos
   

move_ :: (Game m) => Bool -> (Int, Int)  -> m ()
move_ = curry (void . uncurry move)

moveDown, moveUp, moveLeft, moveRight :: (Game m) => m ()
moveDown = printMessage "MOVE DOWN" >> move_ True (0, 1)
moveUp = printMessage "MOVE UP" >> move_ True (0, -1)
moveLeft = printMessage "MOVE LEFT" >> move_ True (-1, 0)
moveRight = printMessage "MOVE RIGHT" >> move_ True (1, 0)

moveToWordBoundary :: (Game m) => (Int, Int) -> m ()
moveToWordBoundary dvec = do
  pos <- gets playerPosition
  npos <- move False dvec
  if pos == npos
    then return ()
    else moveToWordBoundary dvec

beginningOfWord :: (Game m) => m ()
beginningOfWord = do
  printMessage "JUMP TO HEAD"
  backwardVector >>= moveToWordBoundary

endOfWord :: (Game m) => m ()
endOfWord = do
  printMessage "JUMP TO END"
  forwardVector >>= moveToWordBoundary
  
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
moveBack = backwardVector >>= move_ True

moveForward :: (Game m) => m ()
moveForward = forwardVector >>= move_ True

blankCurrentCell :: (Game m) => m ()
blankCurrentCell = get >>= \GameState{..} -> setCellM playerPosition Empty

fillCurrentCell :: (Game m) => Maybe Char -> m ()
fillCurrentCell mchr = do
  GameState{..} <- get
  Just curCell <- getCurrentCell
  case mchr of
   Nothing -> case curCell of
               Empty -> getPreviousCoords >>= (`setCellM` Empty) >> moveBack
               Filled _ -> setCellM playerPosition Empty
   Just c -> setCellM playerPosition (Filled $ toUpper c) >> moveForward

showErrors :: (Game m) => m ()
showErrors = do
  modify $ \s@GameState{..} -> s { shouldShowErrors = not shouldShowErrors }

shutdown :: (MonadIO m) => m ()
shutdown = liftIO (putStrLn "Good bye!" >> exitSuccess)

printCurrentClue :: forall m. (Game m) => m ()
printCurrentClue = getCurrentClue >>= maybe (printMessage "No clue found!") printClue
  where
    getCurrentClue :: m (Maybe Clue)
    getCurrentClue = do
      GameState{..} <- get
      let cluePos :: (Int, Int)
          cluePos = startOfWord playerPosition playerDirection currentBoard
      clues <- asks cluesByCoord >>= return . (M.! cluePos)
      return $ find ((== playerDirection) . direction) clues

printDirection :: (Game m) => m ()
printDirection = do
  dir <- gets playerDirection
  liftIO (clearLine >> (putStrLn . map toUpper $ show dir))

playGame :: (MonadIO m) => FilePath -> m ()
playGame puzFile = do
  res <- runExceptT (loadGame puzFile >>= \(p, s) -> runRWST runGame p s)
  case res of
   Left err -> liftIO $ print err
   Right _ -> shutdown
