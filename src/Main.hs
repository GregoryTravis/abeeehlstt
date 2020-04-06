module Main where

import Sound
import Util

main = do
  s <- readSound "pian.wav"
  msp $ numFrames s
  msp "hi"
