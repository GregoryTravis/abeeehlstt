module Pitcher
( spectralCentroidSeriesFile ) where

import Data.Complex
--import Data.List.Split (chunksOf)
import qualified Data.StorableVector as SV

import Sound
import Util

theBufSize = 2048
theHopSize = 64

-- Compute the spectral centroid of each block of samples.
spectralCentroidSeriesFile :: FilePath -> IO [Double]
spectralCentroidSeriesFile file = do
  sound <- readSound file
  return (spectralCentroidSeries sound)

spectralCentroidSeries :: Sound -> [Double]
spectralCentroidSeries sound = do
  let Sound { samples = samplesVec } = sound
      samples :: [Double]
      samples = map realToFrac (SV.unpack samplesVec)
      bufs :: [[Double]]
      bufs = hopBufs theBufSize theHopSize samples
      complexBufs :: [[Complex Double]]
      complexBufs = map (map (:+ 0)) bufs
   in normalizeSeries $ map spectralCentroid (map fft complexBufs)

-- Normalize numbers from min..max to 0..1
-- Pretends there are no negative numbers
normalizeSeries :: [Double] -> [Double]
normalizeSeries xs = map norm xs
  where mn = minimum xs
        mx = maximum xs
        norm x = (x-mn)/(mx-mn)

-- Return sublists of size 'bufSize', at offsets separated by 'hopSize'
-- TODO: deal with final undersized fragment, which we are currently skipping
-- TODO: use SVs
hopBufs :: Int -> Int -> [a] -> [[a]]
hopBufs bufSize hopSize xs = loop xs
  where loop xs | bufSize <= length xs = take bufSize xs : loop (drop bufSize xs)
        loop _ | otherwise = []

-- Since we are only comparing these values, the frequency bin doesn't matter,
-- so we just use the array index
spectralCentroid :: [Complex Double] -> Double
spectralCentroid cs = eesp debug $ sum (zipWith (*) indices normXS) / sum indices
  where indices = take (length xs) [0..]
        xs = map magnitude cs
        normXS = map (/ totalMag) xs
        totalMag = sum xs
        debug = ("totalMag", totalMag)

-- Taken from https://rosettacode.org/wiki/Fast_Fourier_transform#Haskell
-- Cooley-Tukey
fft :: RealFloat a => [Complex a] -> [Complex a]
fft [] = []
fft [x] = [x]
fft xs = zipWith (+) ys ts ++ zipWith (-) ys ts
    where n = length xs
          ys = fft evens
          zs = fft odds 
          (evens, odds) = split xs
          split [] = ([], [])
          split [x] = ([x], [])
          split (x:y:xs) = (x:xt, y:yt) where (xt, yt) = split xs
          ts = zipWith (\z k -> exp' k n * z) zs [0..]
          exp' k n = cis $ -2 * pi * (fromIntegral k) / (fromIntegral n)
--main = mapM_ print $ fft [1,1,1,1,0,0,0,0]
