module Aubio
( aubioTrack
, aubioOnset
, aubioPitch
, barBeat ) where

import Sound.File.Sndfile as SF hiding (hGetContents)

import External
import Util

parseAubioOutput :: String -> [[String]]
parseAubioOutput s = map words (lines s)

aubioOnset :: FilePath -> IO [Int]
aubioOnset file = do
  nums <- fmap parseAubioOutput $ readFromProc "aubio" ["onset", file]
  ttf <- fileTimeToFrame file
  let onsets = map ttf $ map read $ map one nums
  return onsets
  where one [x] = x

-- (frame, pitch)
aubioPitch :: FilePath -> IO [(Int, Double)]
aubioPitch file = do
  nums <- fmap parseAubioOutput $ readFromProc "aubio" ["pitch", file]
  ttf <- fileTimeToFrame file
  let parse [timeS, pitchS] = (ttf $ read timeS, read pitchS)
  return $ map parse nums

fileTimeToFrame :: FilePath -> IO (Double -> Int)
fileTimeToFrame file = do
  sampleRate <- getSampleRate file
  let toFrame :: Double -> Int
      toFrame t = floor (t * (fromIntegral sampleRate))
  return toFrame

aubioTrack :: String -> IO [Int]
aubioTrack file = do
  s <- readFromProc "aubiotrack" [file]
  info <- getFileInfo file
  let sampleRate = samplerate info
  let toFrame :: Double -> Int
      toFrame t = floor (t * (fromIntegral sampleRate))
  return $ map (\row -> case row of [fs] -> toFrame ((read fs) :: Double)) (parseAubioOutput s)

barBeat :: String -> IO [Int]
barBeat filename = do
  info <- getFileInfo filename
  let sampleRate = samplerate info
  let toFrame :: Double -> Int
      toFrame t = floor (t * (fromIntegral sampleRate))
  csv <- csvCommand "bin/sonic-annotator" ["-q", "-d", "vamp:qm-vamp-plugins:qm-barbeattracker:bars", filename, "-w", "csv", "--csv-stdout"]
  return $ map (toFrame . read . (!! 1)) csv

getSampleRate :: FilePath -> IO Int
getSampleRate file = do
  info <- getFileInfo file
  return $ samplerate info
