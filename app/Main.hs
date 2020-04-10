module Main where

import Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import Data.Word (Word8, Word32)
import Text.Read (readMaybe)

import Lib

sampleRate :: Int
sampleRate = 44000

generate :: Int -> Word8 -> B.ByteString
generate size vol = B.pack $ take size $ cycle [0, vol]

-- Make sure to have a finite waveform here
amplify :: Word8 -> B.ByteString -> B.ByteString
amplify maxVol wave = B.map f wave
  where
    relativeMax :: Word32
    relativeMax = fromIntegral $ B.maximum wave

    f sample = fromIntegral $ div (fromIntegral sample * fromIntegral maxVol) relativeMax

waveForm :: Int -> B.ByteString
waveForm size =
  B.pack $ [0..(n-1)] ++ if n >= 3 then [(n-2),(n-3)..1] else []
  where
    n = fromIntegral $ div size 2 + 1

-- number of bytes in one wave * waves per second(or frequency) = sample rate
frequency :: Int -> Int -> B.ByteString
frequency f num = B.take (fromIntegral num) $ B.cycle $ waveForm bytesInOneWave
  where
    bytesInOneWave = div sampleRate f

waveFile :: Word8 -> B.ByteString -> Put
waveFile vol fileData = do
  let subchunk2Size = B.length fileData

  -- RIFF Header
  putWord32be 0x52494646

  -- ChunkSize
  putWord32le $ fromIntegral $ 36 + subchunk2Size

  -- WAVE
  putWord32be 0x57415645

  -- 'fmt '
  putWord32be 0x666d7420

  -- Subchunk1Size = 16 for PCM
  putWord32le 16

  -- AudioFormat, PCM = 1
  putWord16le 1

  -- Number of Channels, 1 for Mono
  putWord16le 1

  -- Sample Rate
  putWord32le $ fromIntegral sampleRate

  -- ByteRate = SampleRate * NumChannels * BytesPerSample
  putWord32le $ fromIntegral sampleRate

  -- BlockAlign = NumChannels * BytesPerSample
  putWord16le 1

  -- Bits Per Sample
  putWord16le 8

  -- Subchunk2ID = "data"
  putWord32be 0x64617461

  -- Subchunk2Size
  putWord32le $ fromIntegral subchunk2Size

  -- Actual Data
  putLazyByteString $ amplify vol fileData

main :: IO ()
main = do
  -- someFunc

  -- putStrLn "Enter Volume(max 255):"
  -- volStr <- getLine

  -- case readMaybe volStr :: Maybe Word8 of
  --   Just vol -> do

  --     putStrLn "Enter frequency(max 4000):"
  --     freqStr <- getLine

  --     case readMaybe freqStr of
  --       Just freq -> do
  --         let bstr = runPut $ waveFile sampleRate vol freq
  --         B.writeFile "test.wav" bstr
  --         putStrLn "Volume and Frequency accepted, your wav file has been generated"
  --       Nothing ->
  --         putStrLn "You entered an invalid integer for frequency."
  --   Nothing ->
  --     putStrLn "You entered an invalid integer for volume."

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
  B.writeFile "test.wav" $ runPut $ waveFile 127 $ B.concat $ map (`frequency` sampleRate) frequencies