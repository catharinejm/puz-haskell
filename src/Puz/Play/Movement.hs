module Puz.Play.Movement where

import qualified Data.Map.Strict as M
import           Puz.Prelude
import           Puz.Printer
import           Puz.Types
import           Puz.Util

setDirection :: (Game m) => Direction -> m ()
setDirection dir = modify $ \s -> s { playerDirection = dir }

setPosition :: (Game m) => (Int, Int) -> m ()
setPosition pos = modify $ \s -> s { playerPosition = pos }

toggleDirection :: (Game m) => m ()
toggleDirection = do
  GameState{playerDirection} <- get
  let dir = case playerDirection of
             Across -> Down
             Down -> Across
  setDirection dir

jumpToClue :: (Game m) => m ()
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
