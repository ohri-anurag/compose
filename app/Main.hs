module Main where

import Data.Maybe (mapMaybe)

import MusicStream
import WaveAdapter

main :: IO ()
main = do
{-
  A          220
  A#         233.082
  B          246.942
  C          261.626
  C#         277.183
  D          293.665
  D#         311.127
  E          329.628
  F          349.228
  F#         369.994
  G          391.995
  G#         415.305
  A          440
  A#         466.163764
  B          493.883307
  C          523.251139
-}

  let frequencies = [262, 294, 330, 349, 392, 440, 494, 523]

  let bstr = mconcat $ mapMaybe (\f -> createBasicMusicStream 44000 1 f 1 1) frequencies
  process "test.wav" bstr 
  putStrLn "All Done!!"