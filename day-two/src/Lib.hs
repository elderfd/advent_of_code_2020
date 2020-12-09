module Lib
    ( solve
    ) where

import Data.List
import Data.List.Split (splitOn)

data Rule = Rule {
    character :: Char,
    min :: Int,
    max :: Int
}

readInput :: String -> IO [(Rule, String)]
readInput filePath = do
  inputs <- fmap lines (readFile filePath)
  return $ map lineToRuleAndPassword inputs

lineToRuleAndPassword :: String -> (Rule, String)
lineToRuleAndPassword line =
    let [rangeStr, letterPart, password] = words line
        [min, max] = fmap read (splitOn "-" rangeStr)
    in
        (Rule { character = head letterPart, Lib.min = min , Lib.max = max }, password)

checkPasswordAgainstRule :: String -> Rule -> Bool
checkPasswordAgainstRule password rule = 
    let groupedChars = [c | c <- group $ sort password, head c == character rule]
        occurrences = case groupedChars of
            [] -> 0
            _ -> length $ head groupedChars
    in occurrences >= Lib.min rule && occurrences <= Lib.max rule

countValidPasswords :: [(Rule, String)] -> Int
countValidPasswords rulesAndPasswords = 
    let valid = filter (\(rule, pw) -> checkPasswordAgainstRule pw rule) rulesAndPasswords
    in length valid

solve :: String -> IO Int
solve inputPath = do
    inputs <- readInput inputPath
    return $ countValidPasswords inputs
