module Main where

import System.Environment
import Lib

main :: IO ()
main = do
  args <- getArgs
  result <- solve $ head args
  print result