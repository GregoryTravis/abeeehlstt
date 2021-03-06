module Sort
( PitchedNote(..)
, writeNotesToFiles
, chopAtOnsets
, showOnsetsAndPitches
, sortOeuvre ) where

import Data.List (sortOn)
import Data.List.Split (splitOn)
import System.Directory (doesFileExist, listDirectory)
import System.IO.Temp (withSystemTempDirectory)

import Ascii
import Aubio
import Sound
import Util

-- We define the end of a note as the start of the next one; if there isn't a
-- next one, we guess that the note is this long
durationGuess = 44100 `div` 4

-- start, end, pitch (start + end are in frames)
data PitchedNote = PitchedNote Int Int Double
  deriving Show

-- Get all the pitched notes in a file using 'aubio onset' and 'aubio pitch'
getPitchedNotesOP :: FilePath -> IO [PitchedNote]
getPitchedNotesOP file = do
  onsets <- aubioOnset file
  pitches <- aubioPitch file
  return $ timeZip onsets pitches

-- Get all the pitched notes in a file using 'aubio notes'
getPitchedNotesN :: FilePath -> IO [PitchedNote]
getPitchedNotesN file = do
  notes <- aubioNotes file
  return $ map toPn notes
    where toPn (p, s, e) = PitchedNote s e p

-- For each onset, look it up in the pitches. Since they're both sorted we can
-- walk down them at the same time. Don't use the next pitch right after the onset,
-- use one N further on (N=4)
timeZip :: [Int] -> [(Int, Double)] -> [PitchedNote]
timeZip (o:os) pitches@((t0,p0):(t1,p1):ps) | t0 <= o && o < t1 = pn : theRest
  where pn = PitchedNote o (endOf o os) thePitch
        --thePitch = p0
        thePitch = snd $ pitches !! 4
        theRest = timeZip os ((t1,p1):ps)
timeZip (o:os) ((t0,p0):(t1,p1):ps) | t1 <= o = timeZip (o:os) ((t1,p1):ps)
timeZip [] _ = []

-- Given a time and a list of the times after it, return the next time, which
-- is the first element of the list if there is one, and is a guess otherwise
endOf :: Int -> [Int] -> Int
endOf s (e:j) = e
endOf s _ = s + durationGuess

writeNotesToFiles :: FilePath -> FilePath -> IO ()
writeNotesToFiles file destDir = do
  sound <- readSound $ esp file
  pns <- getPitchedNotesOP file
  --mapM putStrLn (map show pns)
  mapM_ (writePitchedNote sound destDir) (zip [0..] pns)

writePitchedNote :: Sound -> FilePath -> (Int, PitchedNote) -> IO ()
writePitchedNote sound destDir (inc, (PitchedNote s e pitch)) = do
  let destFile = destDir ++ "/" ++ (show pitch) ++ "-" ++ (show inc)
  exists <- doesFileExist destFile
  massert (show pitch) (not exists)
  let subSound = snip s e sound
  writeSound destFile subSound
  --msp ("write", destFile)

sortedRecombine :: FilePath -> FilePath -> IO ()
sortedRecombine notesDir outfile = do
  noteFiles <- fmap (map addDir) $ fmap sortNoteFiles $ listDirectory notesDir
  --msp noteFiles
  --msp ("why", noteFiles)
  sounds <- mapM readSound noteFiles
  let all = appendSounds sounds
  writeSound outfile all
    where addDir f = notesDir ++ "/" ++ f

type NoteFilename = (Double, Int)

parseNoteFilename :: String -> NoteFilename
parseNoteFilename s = parse (splitOn "-" s)
  where parse [pitchS, incS] = (read pitchS, read incS)

sortNoteFiles :: [String] -> [String]
sortNoteFiles = sortOn parseNoteFilename

sortOeuvre :: [FilePath] -> FilePath -> IO ()
sortOeuvre inputFiles outputFile = do
  withSystemTempDirectory "abeeehlstt" $ \dir -> do
    mapM (flip writeNotesToFiles dir) inputFiles
    sortedRecombine dir outputFile

-- Divide the audio by onset and write the elements to a dir
chopAtOnsets :: FilePath -> FilePath -> IO ()
chopAtOnsets file dir = do
  --ttf <- fileTimeToFrame file
  onsets <- aubioOnset file
  sound <- readSound file
  --let onsets = map ttf onsetsSeconds
  mapM_ (writeIt sound) $ zip3 [0..] onsets (tail onsets)
  where writeIt sound (i, s, e) = do
          let subSound = snip s e sound
              outFile = dir ++ "/" ++ (show i) ++ ".wav"
          writeSound outFile subSound

interleaveTimeSeries :: (Show a, Show b, Num n, Ord n) => [(n, a)] -> [(n, b)] -> [(n, (Either a b))]
interleaveTimeSeries ((ta,a):as) ((tb,b):bs)
  | ta <= tb = (ta, Left a) : interleaveTimeSeries as ((tb,b):bs)
  | otherwise = (tb, Right b) : interleaveTimeSeries ((ta,a):as) bs
interleaveTimeSeries as [] = map (\(t, a) -> (t, Left a)) as
interleaveTimeSeries [] bs = map (\(t, b) -> (t, Right b)) bs

-- Display two time series as columns, sorted by the values produced by the time-getter functions
interleaveTimeSeriesGrid :: (Show a, Show b, Num n, Ord n) => [a] -> [b] -> (a -> n) -> (b -> n) -> String
interleaveTimeSeriesGrid as bs getATime getBTime =
  let aTimes = map getATime as
      bTimes = map getBTime bs
      --combined :: [Either a b]
      combined = map snd $ interleaveTimeSeries (zip aTimes as) (zip bTimes bs)
      rows :: [[String]]
      rows = map toRow combined
   in stringGrid rows
  where --toRow :: Either a b -> [String]
        toRow (Left a) = [show a, ""]
        toRow (Right b) = ["", show b]

showOnsetsAndPitches :: FilePath -> IO String
showOnsetsAndPitches file = do
  onsets <- aubioOnset file
  pitches <- aubioPitch file
  return $ interleaveTimeSeriesGrid onsets pitches id fst
