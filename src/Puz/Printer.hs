module Puz.Printer where

import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import           Puz.Prelude hiding (reset)
import           Puz.Types
import           Puz.Util
import           System.Console.ANSI
import           System.IO

resetScreen :: (MonadIO m) => m ()
resetScreen = liftIO (setSGR [Reset] >> setCursorPosition 0 0)

reset :: (MonadIO m) => m ()
reset = liftIO $ setSGR [Reset]

setColor :: (MonadIO m) => Color -> m ()
setColor color = liftIO $ setSGR [SetColor Foreground Dull color]

setBackground :: (MonadIO m) => Color -> m ()
setBackground color = liftIO $ setSGR [SetColor Background Dull color]

setIntensity :: (MonadIO m) => ConsoleIntensity -> m ()
setIntensity intensity = liftIO $ setSGR [SetConsoleIntensity intensity]

printSolution :: (Game m) => m ()
printSolution = do
  Puzzle{solution} <- ask
  printBoard Nothing False solution

printPlayerBoard :: (Game m) => m ()
printPlayerBoard = do
  GameState{playerPosition, currentBoard} <- get
  printBoard (Just playerPosition) False currentBoard

data ColorOpts = ColorOpts { foregroundColor :: !Color
                           , backgroundColor :: !Color
                           , colorIntensity :: !ColorIntensity
                           , consoleIntensity :: !ConsoleIntensity
                           }
               deriving (Show)

defaultColors :: ColorOpts
defaultColors = ColorOpts { foregroundColor = White
                          , backgroundColor = Black
                          , colorIntensity = Dull
                          , consoleIntensity = NormalIntensity
                          }

printColor :: (MonadIO m) => ColorOpts -> String -> m ()
printColor ColorOpts{..} str = do
  liftIO $ do
    setSGR [ SetColor Foreground colorIntensity foregroundColor
           , SetColor Background colorIntensity backgroundColor
           , SetConsoleIntensity consoleIntensity
           ]
    putStr str
  reset

brightColors, playerColors, filledColors, whiteBgColors :: ColorOpts
brightColors = defaultColors { foregroundColor = Black, backgroundColor = Cyan }
playerColors = defaultColors { backgroundColor = Yellow, foregroundColor = Black }
filledColors = defaultColors { consoleIntensity = BoldIntensity }
whiteBgColors = defaultColors { foregroundColor = Black
                              , backgroundColor = White
                              , consoleIntensity = BoldIntensity
                              }

printBright :: (MonadIO m) => String -> m ()
printBright = printColor brightColors

printCurrent :: (MonadIO m) => String -> m ()
printCurrent = printColor playerColors

printFilled :: (MonadIO m) => String -> m ()
printFilled = printColor filledColors

printWhite :: (MonadIO m) => String -> m ()
printWhite = printColor whiteBgColors

printAtRow :: (MonadIO m) => String -> Int -> m ()
printAtRow msg row = liftIO $ do
  setCursorPosition row 0
  clearLine
  putStr msg
  liftIO $ hFlush stdout

printMessage :: (Game m) => String -> m ()
printMessage msg = asks messageRow >>= printAtRow msg

printClue :: (Game m) => Clue -> m ()
printClue Clue{..} = asks clueRow >>= printAtRow clueStr
  where
    clueStr = show number ++ " " ++ show direction ++ ": " ++ text

printBoard :: forall m. (Game m) => Maybe (Int, Int) -> Bool -> Board -> m ()
printBoard mCoords showClues Board{rows, width} = do
  printBorder
  mapM_ printRow (V.toList rows `zip` [0..])
  where
    printBorder = printBright "+" >> printDashes >> printBright "+" >> endl
    printDashes = replicateM_ (4*width-1) (printBright "-")
    printRow (row, y) = printBright "|" >> mapM_ printCell rowWithCoords >> endl >> printBorder
      where rowWithCoords = V.toList row `zip` ([0..] `zip` repeat y)
    printCell :: (Cell, (Int, Int)) -> m ()
    printCell (Blocked, _) = prn "   " >> printBright "|"
    printCell (Empty, coords) = do
      let p = if isPlayerCoords coords then printCurrent else printWhite
      printClueOrContent coords p "   "
      printBright "|"
    printCell (Filled ch, coords) = do
      let baseColors = if isPlayerCoords coords then playerColors else whiteBgColors
      drawError <- shouldDrawError ch coords
      let colors = baseColors { foregroundColor = if drawError then Red else Black }
      printClueOrContent coords (printColor colors) [' ', ch, ' ']
      printBright "|"
    isPlayerCoords c = maybe False (== c) mCoords
    printClueOrContent coords pfn content = do
      clues <- asks cluesByCoord
      let mclue = maybeBool showClues >> M.lookup coords clues >>= safeHead
          mcnum = mclue >>= return . take 3 . (++ repeat ' ') . show . number
          str = fromMaybe content mcnum
      pfn str
    prn = liftIO . putStr
    endl = liftIO $ putStrLn ""
    prnCol c s = setColor c >> prn s >> reset
    shouldDrawError :: Char -> (Int, Int) -> m Bool
    shouldDrawError ch coords = do
      showErrs <- gets shouldShowErrors
      if showErrs
        then do Puzzle{..} <- ask
                let mSolCh = getCell coords solution >>= cellChar
                return $ maybe False (/= ch) mSolCh
        else return False

showClueNumbers :: (Game m) => m ()
showClueNumbers = do
  resetScreen
  GameState{playerPosition, currentBoard} <- get
  printBoard (Just playerPosition) True currentBoard

terminalPosition :: (Int, Int) -> (Int, Int)
terminalPosition (x, y) = (1+y*2, x*4) -- (Row, Col)

goToCoords :: (MonadIO m) => (Int, Int) -> m ()
goToCoords = liftIO . uncurry setCursorPosition . terminalPosition

echoOff :: (MonadIO m) => m ()
echoOff = liftIO $ hSetEcho stdout False

echoOn :: (MonadIO m) => m ()
echoOn = liftIO $ hSetEcho stdout True
