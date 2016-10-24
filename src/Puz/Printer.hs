module Puz.Printer where

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

printBright :: (MonadIO m) => String -> m ()
printBright str = liftIO $ do
  setColor Black
  setBackground Cyan
  putStr str
  reset

printSolution :: (MonadReader Puzzle m, MonadIO m) => m ()
printSolution = do
  Puzzle{solution} <- ask
  printBoard Nothing solution

printPlayerBoard :: (MonadState GameState m, MonadIO m) => m ()
printPlayerBoard = do
  GameState{playerPosition, currentBoard} <- get
  printBoard (Just playerPosition) currentBoard

printCurrent :: (MonadIO m) => String -> m ()
printCurrent str = do
  setColor Black
  setBackground Yellow
  liftIO $ putStr str
  reset

printFilled :: (MonadIO m) => String -> m ()
printFilled str = do
  setIntensity BoldIntensity
  setColor White
  liftIO $ putStr str
  reset

printWhite :: (MonadIO m) => String -> m ()
printWhite str = do
  setBackground White
  setColor Black
  setIntensity BoldIntensity
  liftIO $ putStr str
  reset

printMessage :: (MonadIO m) => String -> m ()
printMessage msg = liftIO (clearLine >> putStrLn msg)

printBoard :: (MonadIO m) => Maybe (Int, Int) -> Board -> m ()
printBoard mCoords Board{rows, width} = do
  printBorder
  mapM_ printRow (V.toList rows `zip` [0..])
  where
    printBorder = printBright "+" >> printDashes >> printBright "+" >> endl
    printDashes = replicateM_ (4*width-1) (printBright "-")
    printRow (row, y) = printBright "|" >> mapM_ printCell rowWithCoords >> endl >> printBorder
      where rowWithCoords = V.toList row `zip` ([0..] `zip` repeat y)
    printCell (Blocked, _) = printBright " " >> prn " " >> printBright " |"
    printCell (Empty, coords) = do
      let p = if isPlayerCoords coords then printCurrent else printWhite
      p "   "
      printBright "|"
    printCell (Filled c, coords) = do
      let p = if isPlayerCoords coords then printCurrent else printWhite
      p [' ', c, ' ']
      printBright "|"
    isPlayerCoords c = maybe False (== c) mCoords
    prn = liftIO . putStr
    endl = liftIO $ putStrLn ""
    prnCol c s = setColor c >> prn s >> reset

terminalPosition :: (Int, Int) -> (Int, Int)
terminalPosition (x, y) = (1+y*2, x*4) -- (Row, Col)

goToCoords :: (MonadIO m) => (Int, Int) -> m ()
goToCoords = liftIO . uncurry setCursorPosition . terminalPosition

printLocation :: (Game m) => m ()
printLocation = do
  Just cell <- getCurrentCell
  GameState{playerPosition} <- get
  goToCoords playerPosition
  setColor Black
  setBackground Yellow
  printCell cell
  reset
  liftIO $ hFlush stdout
  where
    printCell Empty = liftIO $ putStr " "
    printCell Blocked = liftIO $ putStr "#"
    printCell (Filled c) = liftIO $ putStr [c]
    
