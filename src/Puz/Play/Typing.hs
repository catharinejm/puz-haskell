module Puz.Play.Typing where

import Puz.Play.Movement
import Puz.Prelude
import Puz.Types
import Puz.Util

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
