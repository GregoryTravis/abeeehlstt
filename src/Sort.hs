module Sort
( PitchedNote(..)
, getPitchedNotes
, writeNotesToFiles
, sortOeuvre ) where

import Data.List (sortOn)
import System.Directory (doesFileExist, listDirectory)

import Aubio
import Sound
import Util

-- We define the end of a note as the start of the next one; if there isn't a
-- next one, we guess that the note is this long
durationGuess = 44100 `div` 4

-- start, end, pitch (start + end are in frames)
data PitchedNote = PitchedNote Int Int Double
  deriving Show

-- Get all the pitched notes in a file
getPitchedNotes :: FilePath -> IO [PitchedNote]
getPitchedNotes file = do
  onsets <- aubioOnset file
  pitches <- aubioPitch file
  return $ timeZip onsets pitches

-- For each onset, look it up in the pitches. Since they're both sorted we can
-- walk down them at the same time
timeZip :: [Int] -> [(Int, Double)] -> [PitchedNote]
timeZip (o:os) ((t0,p0):(t1,p1):ps) | t0 <= o && o < t1 = pn : theRest
  where pn = PitchedNote o (endOf o os) p0
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
  pns <- getPitchedNotes file
  mapM_ (writePitchedNote sound destDir) pns

writePitchedNote :: Sound -> FilePath -> PitchedNote -> IO ()
writePitchedNote sound destDir (PitchedNote s e pitch) = do
  let destFile = destDir ++ "/" ++ (show pitch)
  exists <- doesFileExist destFile
  massert (show pitch) (not exists)
  let subSound = snip s e sound
  writeSound destFile subSound

sortedRecombine :: FilePath -> FilePath -> IO ()
sortedRecombine notesDir outfile = do
  noteFiles <- fmap (map addDir) $ fmap sortAsDoubles $ listDirectory notesDir
  msp noteFiles
  msp ("why", noteFiles)
  sounds <- mapM readSound noteFiles
  let all = appendSounds sounds
  writeSound outfile all
    where addDir f = notesDir ++ "/" ++ f

sortAsDoubles :: [String] -> [String]
sortAsDoubles = sortOn (read :: String -> Double)

sortOeuvre :: [FilePath] -> FilePath -> IO ()
sortOeuvre inputFiles outputFile = do
  let dir = "notes"
  mapM (flip writeNotesToFiles dir) inputFiles
  sortedRecombine dir outputFile
