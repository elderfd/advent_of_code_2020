{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( solve,
  )
where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

readInput :: String -> IO [Int]
readInput filePath = do
  fileContent <- readFile filePath
  let values = (mapMaybe readMaybe :: [String] -> [Int]) (lines fileContent)
  return values

getCorrectNumbers :: [Int] -> Maybe (Int, Int)
getCorrectNumbers numbers =
  let summed = map (\x -> map (+ x) numbers) numbers
      matchIndices = [(x, y) | (y, row) <- zip ([0 ..] :: [Int]) summed, (x, value) <- zip ([0 ..] :: [Int]) row, value == 2020]
      firstMatch :: Maybe (Int, Int) = case matchIndices of
        [] -> Nothing
        (x : _) -> Just (numbers !! fst x, numbers !! snd x)
   in firstMatch

getFinalAnswer :: (Int, Int) -> Int
getFinalAnswer numbers = fst numbers * snd numbers

-- case numbers of
-- Nothing -> Nothing
-- Just (first, second) -> Just (first * second)

solve :: String -> IO (Maybe Int)
solve filePath = do
  inputNumbers <- readInput filePath
  return (fmap getFinalAnswer (getCorrectNumbers inputNumbers))
