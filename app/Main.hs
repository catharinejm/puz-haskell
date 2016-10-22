module Main where

import Puz

main :: IO ()
main = do
  args <- getArgs
  print args
  if length args /= 1
    then do putStrLn "Usage: puz <path/to.puz>"
            exitFailure
    else playGame (head args)
