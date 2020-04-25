{-# LANGUAGE LambdaCase #-}
module MusicStream where

import qualified Data.ByteString.Lazy as B
import Data.Maybe (mapMaybe)
import Data.Ratio
import qualified Data.Vector as V
import Data.Word (Word8)
import qualified Safe as S

type DataVector = V.Vector Double

newtype SingleChannel = SC
    { stream :: DataVector
    } deriving (Eq, Show)

data DualChannel = DC
    { left :: DataVector
    , right :: DataVector
    } deriving (Eq, Show)

data Channel
    = SingleChannel SingleChannel
    | DualChannel DualChannel
    deriving (Eq, Show)

data MusicStream = MusicStream
    { samplingRate :: Int
    , bytesPerSample :: Int
    , channel :: Channel
    }

instance Semigroup SingleChannel where
    SC x <> SC y = SC $ x <> y

instance Semigroup DualChannel where
    DC u v <> DC w x = DC (u <> w) (v <> x)

instance Semigroup Channel where
    SingleChannel x <> SingleChannel y =
        SingleChannel $ x <> y
    DualChannel x <> DualChannel y =
        DualChannel (x <> y)
    SingleChannel (SC x) <> DualChannel (DC y z) =
        DualChannel (DC (x <> y) (x <> z))
    DualChannel (DC x y) <> SingleChannel (SC z) =
        DualChannel (DC (x <> z) (y <> z))

instance Semigroup MusicStream where
    -- TODO: For now assume that both have equal samplingRate and bytesPerSample
    x <> y = MusicStream
        { samplingRate = samplingRate x
        , bytesPerSample = bytesPerSample x
        , channel = channel x <> channel y
        }

instance Monoid MusicStream where
    mempty = MusicStream 1 1 (SingleChannel $ SC V.empty)

isDualChannelStream :: Channel -> Bool
isDualChannelStream (DualChannel _) = True
isDualChannelStream _ = False

simpleSineWave :: Int -> Double -> Double -> Int -> Maybe Channel
simpleSineWave samplingRate frequency duration channels
    | channels == 1 =
        Just $ SingleChannel $ SC channel
    | channels == 2 =
        Just $ DualChannel $ DC channel channel
    | otherwise =
        Nothing
    where
        channel = V.generate (round $ fromIntegral samplingRate * duration) (\i -> waveFn $ fromIntegral i * period)

        period = 1 % samplingRate

        waveFn t = sin (2 * pi * frequency * (fromIntegral (numerator t) / fromIntegral (denominator t)))

standingWave :: Int -> Double -> Double -> Int -> Maybe Channel
standingWave samplingRate frequency duration channels
    | channels == 1 =
        Just $ SingleChannel $ SC channel
    | channels == 2 =
        Just $ DualChannel $ DC channel channel
    | otherwise =
        Nothing
    where
        n = 10.0

        channel = V.map waveFn $ V.generate (round $ fromIntegral samplingRate * duration) (\i -> fromIntegral i * period)

        period = 1 % samplingRate

        waveFn t = (/ n) $ sum $ map (\i -> (1.0 / i) * sin (pi * i / (2 * (n + 1))) * timeWave i t) [1..n]

        timeWave i t = sin (2 * pi * i * frequency * (fromIntegral (numerator t) / fromIntegral (denominator t)))

createBasicMusicStream
    :: Int          -- samplingRate
    -> Int          -- bytesPerSample
    -> Double       -- frequency
    -> Double       -- duration
    -> Int          -- channels
    -> Maybe MusicStream
createBasicMusicStream samplingRate bytesPerSample frequency duration channels =
    MusicStream samplingRate bytesPerSample <$> channel
    where
        channel = standingWave samplingRate frequency duration channels

convertSingleToDouble :: SingleChannel -> DualChannel
convertSingleToDouble (SC v) = DC v v

superimpose :: [MusicStream] -> [Int] -> Maybe MusicStream
superimpose [] _ = Nothing
superimpose musicStreams@(x:_) weights =
    -- TODO: Assume that sampling rates and bytes per sample are equal
    let
        rate = samplingRate x
        bps = bytesPerSample x

        streams = map channel musicStreams

        singleStreams = mapMaybe (\case
            SingleChannel (SC s) -> Just s
            DualChannel _ -> Nothing
            ) streams
        dualStreams = mapMaybe (\case
            SingleChannel _ -> Nothing
            DualChannel (DC l r) -> Just (l, r)
            ) streams

        headMay v
            | V.null v  = Nothing
            | otherwise = Just $ V.head v

        tailMay v
            | V.null v  = Nothing
            | otherwise = Just $ V.tail v

        addStreams = V.fromList . addStreams'

        addStreams' xss
            | null heads = []
            | otherwise  = sum weightedValues / weightSum : addStreams' (mapMaybe tailMay xss)
            where
                heads = mapMaybe headMay xss

                weightedValues = zipWith (\ a b -> a * fromIntegral b) heads weights

                weightSum = fromIntegral $ sum weights
    in
        Just . MusicStream rate bps $
            if length singleStreams == length musicStreams
                then SingleChannel $ SC $ addStreams singleStreams
                else DualChannel $ DC (addStreams $ map fst dualStreams) (addStreams $ map snd dualStreams)


