module Ascii
( stringGrid ) where

import Data.List (intercalate, intersperse, transpose)

import Util

stringGrid :: [[String]] -> String
stringGrid rows =
  let --cols :: [[a]]
      cols = transpose rows
      --paddedCols :: [[a]]
      paddedCols = map padStrings cols
      --paddedRows :: [[a]]
      paddedRows = transpose paddedCols
      widths :: [Int]
      widths = map (length . (!!0)) paddedCols
      hbar = makeHBar widths
      asciiRows :: [String]
      asciiRows = map makeRow paddedRows
      debug = (widths, hbar)
   in stackStrings ([hbar] ++ intersperse hbar asciiRows ++ [hbar])

-- Add newlines between and after the strings
stackStrings :: [String] -> String
stackStrings ss = (intercalate "\n" ss) ++ "\n"

-- One of these: +---+--+-----+
makeHBar :: [Int] -> String
makeHBar widths = outercalate "+" dasheses
  where dasheses = map dashes widths
        dashes n = take n (repeat '-')

-- One of these: |abc|de|fghij|
makeRow :: [String] -> String
makeRow = outercalate "|"

-- Append spaces to make them all the same length
padStrings :: [String] -> [String]
padStrings ss =
  let maxWidth = maximum (map length ss)
   in map (padTo maxWidth) ss

padTo :: Int -> String -> String
padTo n s = s ++ (take (n - length s) (repeat ' '))
