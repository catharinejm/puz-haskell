module Puz.Printer where

import           Puz.Prelude hiding (reset)
import           Puz.Types
import           Puz.Util
import           System.Console.ANSI

resetScreen :: (MonadIO m) => m ()
resetScreen = liftIO (clearScreen >> setSGR [Reset] >> setCursorPosition 0 0)

reset :: (MonadIO m) => m ()
reset = liftIO $ setSGR [Reset]

setColor :: (MonadIO m) => Color -> m ()
setColor color = liftIO $ setSGR [SetColor Foreground Dull color]

setBackground :: (MonadIO m) => Color -> m ()
setBackground color = liftIO $ setSGR [SetColor Background Dull color]

setIntensity :: (MonadIO m) => ConsoleIntensity -> m ()
setIntensity intensity = liftIO $ setSGR [SetConsoleIntensity intensity]

printInverted :: (MonadIO m) => String -> m ()
printInverted str = liftIO $ do
  setColor Black
  setBackground Cyan
  putStr str
  reset

printBoardBy :: (MonadState Puzzle m, MonadIO m) => (Puzzle -> Board) -> m ()
printBoardBy f = getPuz f >>= printBoard

printBoard :: (MonadIO m) => Board -> m ()
printBoard Board{rows, width} = do
  printBorder
  mapM_ printRow rows
  where
    printBorder = printInverted "+" >> replicateM_ (4*width-1) (printInverted "-") >> printInverted "+" >> endl
    printRow row = printInverted "|" >> mapM_ printCell row >> endl >> printBorder
    printCell Blocked = printInverted " " >> prn " " >> printInverted " |"
    printCell Empty = printInverted "   |"
    printCell (Filled c) = do
      setBackground White
      setIntensity BoldIntensity
      setColor Black
      prn [' ', c, ' ']
      reset
      printInverted "|"
    prn = liftIO . putStr
    endl = liftIO $ putStrLn ""
    prnCol c s = setColor c >> prn s >> reset
