module Main where

import Lib (solve)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  result <- solve $ head args
  print result
