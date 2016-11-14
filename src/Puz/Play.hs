module Puz.Play where

import Puz.Play.Movement
import Puz.Play.Typing
import Puz.Play.UI
import Puz.Prelude
import Puz.Printer
import Puz.Types
import Puz.Util
import System.Console.ANSI
import System.IO

play :: forall m. (Game m) => m ()
play = do
  liftIO $ hSetBuffering stdin NoBuffering
  echoOff
  liftIO clearScreen
  step
  where
    step = do
      resetScreen
      printPlayerBoard
      printDirection
      printCurrentClue
      liftIO $ hFlush stdout
      c <- liftIO getChar
      dispatch c
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
       'x' -> setupConsole >> setMode Console
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
