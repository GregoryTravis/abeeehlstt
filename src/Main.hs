module Main where

import System.Directory (listDirectory)

import Aubio
import Sort
import Sound
import Util

main = do
  --chopAtOnsets "beatles/bb0.wav" "bb0notes"

  -- grid <- showOnsetsAndPitches "beatles/bb0.wav"
  -- putStrLn grid

  let outFile = "ainp.wav"
      srcDir = "beatles"
      addDir s = srcDir ++ "/" ++ s
  files <- fmap (map addDir) $ listDirectory srcDir
  sortOeuvre files outFile
  msp "hi"
