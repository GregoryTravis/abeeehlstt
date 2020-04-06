module Main where

import Aubio
import Sort
import Sound
import Util

main = do
  let file = "pian.wav"
      destDir = "notes"
      outFile = "ainp.wav"
  sortOeuvre [file] outFile
  msp "hi"
