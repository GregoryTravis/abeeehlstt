module Sort
( PitchedNote(..)
, getPitchedNotes ) where

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