module Main where

import Data.List (intercalate)
import Data.Maybe (mapMaybe)

import MusicStream
import WaveAdapter

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

  -- let frequencies = [262, 294, 330, 349, 392, 440, 494, 523]

  -- let bstr = mconcat $ mapMaybe (\f -> createBasicMusicStream 44000 1 f 1 1) frequencies
  -- process "test.wav" bstr
  -- putStrLn "All Done!!"
main :: IO ()
main = swatKats

swatKats :: IO ()
swatKats = do
  let
    frequencies =
      [ (130, 0.15)
      , (0  , 0.1) -- break
      , (130, 0.15)
      , (200, 0.15)
      , (0  , 0.25) -- break
      , (220, 0.6)
      -- , (0  , 0.1) -- break
      , (200, 0.15)
      , (175, 0.15)
      , (260, 0.3)
      , (0  , 0.1) -- break
      , (260, 0.75)

      , (0  , 0.35)

      , (130, 0.15)
      , (0  , 0.1) -- break
      , (130, 0.15)
      , (200, 0.15)
      , (0  , 0.25) -- break
      , (220, 0.6)
      -- , (0  , 0.1) -- break
      , (200, 0.15)
      , (175, 0.15)
      , (235, 0.3)
      , (0  , 0.1) -- break
      , (235, 0.75)

      , (0  , 0.2)

      , (270, 0.15)
      , (300, 0.15)
      , (320, 0.5)
      , (0  , 0.1) -- break
      , (320, 0.15)
      , (260, 0.15)
      , (300, 0.5)
      , (0  , 0.1) -- break
      , (240, 0.25)
      , (260, 0.3)
      , (0  , 0.1) -- break
      , (260, 0.6)

      , (0  , 0.4)

      , (270, 0.15)
      , (300, 0.15)
      , (320, 0.5)
      , (0  , 0.1) -- break
      , (300, 0.15)
      , (260, 0.15)
      , (240, 0.5)
      , (0  , 0.1) -- break
      , (240, 0.25)
      , (200, 0.3)
      , (0  , 0.1) -- break
      , (200, 0.6)
      ]
    bass = 
      [ (130, 0.4)
      , (0  , 0.15)
      , (130, 0.15)
      , (0  , 0.1)
      , (130, 0.4)
      , (0  , 0.15)
      , (130, 0.15)
      , (0  , 0.1)
      , (130, 0.4)
      , (0  , 0.15)
      , (130, 0.15)
      , (0  , 0.1)
      , (160, 0.3)
      , (0  , 0.1)
      , (120, 0.3)

      , (0  , 0.1)

      , (130, 0.4)
      , (0  , 0.15)
      , (130, 0.15)
      , (0  , 0.1)
      , (130, 0.4)
      , (0  , 0.15)
      , (130, 0.15)
      , (0  , 0.1)
      , (130, 0.4)
      , (0  , 0.15)
      , (130, 0.15)
      , (0  , 0.1)
      , (160, 0.3)
      , (0  , 0.1)
      , (180, 0.3)
      ]

  let
    bstr1 = mconcat $ mapMaybe (\(f, d) -> createBasicMusicStream 44000 1 (2*f) d 1) frequencies
    bstr2 = mconcat $ mapMaybe (\(f, d) -> createBasicMusicStream 44000 1 (2*f) d 1) bass
  case superimpose [bstr1, bstr2] [3,1] of
    Just combined -> process "Swat Kats.wav" combined
    Nothing -> putStrLn "Failed to superimpose"