module Main where

import Aubio
import Sound
import Util

main = do
  let file = "pian.wav"
  s <- readSound file
  msp $ numFrames s
  onsets <- aubioOnset file
  msp onsets
  pitches <- aubioPitch file
  msp pitches
  msp "hi"
