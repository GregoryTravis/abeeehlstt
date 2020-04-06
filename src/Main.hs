module Main where

import Aubio
import Sort
import Sound
import Util

main = do
  let file = "pian.wav"
  -- s <- readSound file
  -- msp $ numFrames s
  -- onsets <- aubioOnset file
  -- msp onsets
  -- pitches <- aubioPitch file
  -- msp pitches
  pns <- getPitchedNotes file
  msp pns
  msp "hi"
