module Aubio
( aubioTrack
, aubioOnset
, aubioPitch
, aubioNotes
, barBeat ) where

import Sound.File.Sndfile as SF hiding (hGetContents)

import External
import Text.Read (readMaybe)
import Util

-- For aubio programs that produce a grid of values
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

checkIsDouble :: String -> Bool
checkIsDouble s = case readMaybe s :: Maybe Double of Just _ -> True
                                                      Nothing -> False

-- Pitch, start frame, end frame
aubioNotes :: FilePath -> IO [(Double, Int, Int)]
aubioNotes file = do
  ttf <- fileTimeToFrame file
  ls <- fmap lines $ readFromProc "aubio" ["notes", file]
  -- First and last lines are just a single double
  massert "" (checkIsDouble (head ls))
  massert "" (checkIsDouble (last ls))
  let lsSorted :: [String]
      lsSorted = reverse (tail (reverse (tail ls)))
      nums :: [[Double]]
      nums = map (map read) $ map words lsSorted
      trip [a, b, c] = (a, ttf b, ttf c)
  return $ map trip nums
  where assertJustDouble s = msp (read s :: Double) -- Not sure how to do this otherwise

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
