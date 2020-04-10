module MusicStream where

import qualified Data.ByteString.Lazy as B
import Data.Ratio
import qualified Data.Vector as V
import Data.Word (Word8)

type DataVector = V.Vector Integer

data Stream
    = SingleChannel DataVector
    | DualChannel DataVector DataVector
    deriving Show

instance Semigroup Stream where
    SingleChannel x <> SingleChannel y =
        SingleChannel $ x <> y
    DualChannel u v <> DualChannel w x =
        DualChannel (u <> w) (v <> x)
    SingleChannel x <> DualChannel y z =
        DualChannel (x <> y) (x <> z)
    DualChannel x y <> SingleChannel z =
        DualChannel (x <> z) (y <> z)


data MusicStream = MusicStream
    { samplingRate :: Int
    , bytesPerSample :: Int
    , stream :: Stream
    }

instance Semigroup MusicStream where
    -- For now assume that both have equal samplingRate and bytesPerSample
    x <> y = MusicStream
        { samplingRate = samplingRate x
        , bytesPerSample = bytesPerSample x
        , stream = stream x <> stream y
        }

instance Monoid MusicStream where
    mempty = MusicStream 1 1 (SingleChannel V.empty)

sampleSineWave :: Int -> Int -> Int -> Int -> Int -> Maybe Stream
sampleSineWave samplingRate bytesPerSample frequency duration channels
    | channels == 1 =
        Just $ SingleChannel stream
    | channels == 2 =
        Just $ DualChannel stream stream
    | otherwise =
        Nothing
    where
        stream = V.map waveFn $ V.generate (samplingRate * duration) (\i -> fromIntegral i * period)

        period = 1 % samplingRate

        amplitude = 2.0 ^ (8 * bytesPerSample - 1) - 1

        waveFn :: Ratio Int -> Integer
        waveFn t = round $ amplitude *
                    sin (2 * pi * fromIntegral frequency * (fromIntegral (numerator t) / fromIntegral (denominator t)))

createBasicMusicStream
    :: Int          -- samplingRate
    -> Int          -- bytesPerSample
    -> Int          -- frequency
    -> Int          -- duration
    -> Int          -- channels
    -> Maybe MusicStream
createBasicMusicStream samplingRate bytesPerSample frequency duration channels =
    MusicStream samplingRate bytesPerSample <$> stream
    where
        stream = sampleSineWave samplingRate bytesPerSample frequency duration channels
