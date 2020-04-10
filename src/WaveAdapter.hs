module WaveAdapter(
    process
) where

import Data.Binary.Put (Put, putLazyByteString, putWord16le, putWord32be, putWord32le, runPut)
import qualified Data.ByteString.Lazy as B
import Data.Int (Int64)
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32)

import MusicStream

process :: String -> MusicStream -> IO ()
process fileName musicStream = do
    let
        bytes = runPut $
            case stream musicStream of
                SingleChannel channel ->
                    waveFile 1 bps sr $ convert channel

                DualChannel channel1 channel2 ->
                    waveFile 2 bps sr (combine (fromIntegral bps) (convert channel1) (convert channel2))

    B.writeFile fileName bytes
    where
        bps = fromIntegral $ bytesPerSample musicStream
        sr = fromIntegral $ samplingRate musicStream

        convert :: DataVector -> B.ByteString
        convert channel
            | bps == 1 =
                B.pack $ map oneByte $ V.toList channel
            | bps == 2 =
                B.concat $ map twoBytes $ V.toList channel
            where
                min = abs $ if V.null channel then 0 else V.minimum channel

                oneByte :: Integer -> Word8
                oneByte i = fromIntegral $ i + min

                twoBytes :: Integer -> B.ByteString
                twoBytes = runPut . putWord16le . fromIntegral

combine :: Int64 -> B.ByteString -> B.ByteString -> B.ByteString
combine num bytes1 bytes2
    | B.null bytes1 = bytes2
    | B.null bytes2 = bytes1
    | otherwise = B.concat [B.take num bytes1, B.take num bytes2, combine num (B.drop num bytes1) (B.drop num bytes2)]

waveFile :: Word16 -> Word16 -> Word32 -> B.ByteString -> Put
waveFile numChannels bytesPerSample sampleRate fileData = do
  let subchunk2Size = fromIntegral $ B.length fileData

  -- RIFF Header
  putWord32be 0x52494646

  -- ChunkSize
  putWord32le $ 36 + subchunk2Size

  -- WAVE
  putWord32be 0x57415645

  -- 'fmt '
  putWord32be 0x666d7420

  -- Subchunk1Size = 16 for PCM
  putWord32le 16

  -- AudioFormat, PCM = 1
  putWord16le 1

  -- Number of Channels, 1 for Mono, 2 for stereo
  putWord16le numChannels

  -- Sample Rate
  putWord32le sampleRate

  -- ByteRate = SampleRate * NumChannels * BytesPerSample
  putWord32le $ sampleRate * fromIntegral numChannels * fromIntegral bytesPerSample

  -- BlockAlign = NumChannels * BytesPerSample
  putWord16le $ numChannels * bytesPerSample

  -- Bits Per Sample
  putWord16le $ 8 * bytesPerSample

  -- Subchunk2ID = "data"
  putWord32be 0x64617461

  -- Subchunk2Size
  putWord32le subchunk2Size

  -- Actual Data
  putLazyByteString fileData