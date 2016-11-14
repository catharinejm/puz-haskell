module Puz.Play.UI where

import Data.Map.Strict ((!))
import Puz.Prelude
import Puz.Printer
import Puz.Types
import Puz.Util
import System.Console.ANSI
import System.IO

setupConsole :: (MonadIO m) => m ()
setupConsole = do
  liftIO $ hSetBuffering stdin LineBuffering
  echoOn
  liftIO clearScreen
  resetScreen

printCurrentClue :: forall m. (Game m) => m ()
printCurrentClue = getCurrentClue >>= maybe (printMessage "No clue found!") printClue
  where
    getCurrentClue :: m (Maybe Clue)
    getCurrentClue = do
      GameState{..} <- get
      let cluePos :: (Int, Int)
          cluePos = startOfWord playerPosition playerDirection currentBoard
      clues <- asks cluesByCoord >>= return . (! cluePos)
      return $ find ((== playerDirection) . direction) clues

printDirection :: (Game m) => m ()
printDirection = do
  dir <- gets playerDirection
  liftIO (clearLine >> (putStrLn . map toUpper $ show dir))
