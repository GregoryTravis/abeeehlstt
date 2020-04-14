module Main where

import System.Directory (listDirectory)

import Aubio
import Pitcher
import Sort
import Sound
import Util

main = do
  --chopAtOnsets "beatles/bb0.wav" "bb0notes"

  -- grid <- showOnsetsAndPitches "beatles/bb0.wav"
  -- putStrLn grid

  -- let outFile = "ainp.wav"
  --     srcDir = "beatles"
  --     addDir s = srcDir ++ "/" ++ s
  -- files <- fmap (map addDir) $ listDirectory srcDir
  -- sortOeuvre files outFile

  centroids <- spectralCentroidSeriesFile "pian.wav" -- "beatles/bb0.wav"
  msp centroids

  msp "hi"
