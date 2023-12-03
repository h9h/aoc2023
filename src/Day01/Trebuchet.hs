-- Day 1: Trebuchet
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day01.Trebuchet (solve) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails)
import Data.Maybe (mapMaybe)

--- Part 1

getFirstAndLastDigit :: [Char] -> (Int, Int)
getFirstAndLastDigit str = (head digits, last digits)
    where digits =  map digitToInt $ filter isDigit str

getCalibration :: String -> Int
getCalibration str = tens * 10 + ones
    where (tens, ones) = getFirstAndLastDigit str

{-
>>> getFirstAndLastDigit "1-53"
(1,3)

>>> getCalibration "1-3"
13
-}

--- Part 2

parseDigit :: String -> Maybe Int
parseDigit = \case
  'z' : 'e' : 'r' : 'o' : _ -> Just 0
  'o' : 'n' : 'e' : _ -> Just 1
  't' : 'w' : 'o' : _ -> Just 2
  't' : 'h' : 'r' : 'e' : 'e' : _ -> Just 3
  'f' : 'o' : 'u' : 'r' : _ -> Just 4
  'f' : 'i' : 'v' : 'e' : _ -> Just 5
  's' : 'i' : 'x' : _ -> Just 6
  's' : 'e' : 'v' : 'e' : 'n' : _ -> Just 7
  'e' : 'i' : 'g' : 'h' : 't' : _ -> Just 8
  'n' : 'i' : 'n' : 'e' : _ -> Just 9
  x : _ | isDigit x -> Just $ digitToInt x
  _ -> Nothing


solve2 :: [String] -> Int
solve2 = sum . map calibration
  where
    firstDigit = head . mapMaybe parseDigit . tails
    lastDigit = head . mapMaybe parseDigit . reverse . tails
    calibration xs = 10 * firstDigit xs + lastDigit xs

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  let numbers = map getCalibration (lines contents)
  print $ sum numbers
  print $ solve2 (lines contents)
