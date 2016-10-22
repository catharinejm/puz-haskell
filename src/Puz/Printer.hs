module Puz.Printer where

import Puz.Prelude
import Puz.Types
import qualified Data.Vector as V

printBoard :: Board -> String
printBoard Board{..} = unlines (printBorder : V.toList (V.map printRow rows))
  where
    printRow row = '|' : (concat $ V.toList (V.map printCell row) ++ ["\n" ++ printBorder])
    printBorder = concat $ ["+"] ++ replicate (2*width-1) "-" ++ ["+"]
    printCell Blocked = "#|"
    printCell Empty = " |"
    printCell (Filled c) = c:"|"
