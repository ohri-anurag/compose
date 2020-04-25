module MusicStreamSpec where

import qualified Data.Vector as V
import Test.Hspec
import Test.QuickCheck

import MusicStream

-- Sine Wave Tests begin
numberOfZeroes :: DataVector -> Int
numberOfZeroes vector
    | V.length vector < 2 =
        0
    | b == 0 =
        1 + numberOfZeroes (V.drop 2 vector)
    | signum a /= signum b =
        1 + numberOfZeroes (V.tail vector)
    | otherwise =
        numberOfZeroes (V.tail vector)
        where
            [a, b] = V.toList $ V.take 2 vector

numberOfWaves :: DataVector -> Int
numberOfWaves vector = div (zeroes - 1) 2
    where zeroes = numberOfZeroes vector

prop_length_sineWave :: Positive Int -> Positive Double -> Positive Double -> Bool
prop_length_sineWave (Positive rate) (Positive freq) (Positive duration) =
    case simpleSineWave rate freq duration 1 of
        Just channel ->
            case channel of
                SingleChannel (SC stream) ->
                    V.length stream == round (fromIntegral rate * duration)

                _ ->
                    False
        Nothing ->
            False

prop_numberOfWaves_sineWave :: Positive Double -> Positive Double -> Bool
prop_numberOfWaves_sineWave (Positive freq) (Positive duration) =
    case simpleSineWave 44000 freq' duration' 1 of
        Just channel ->
            case channel of
                SingleChannel (SC stream) ->
                    numberOfWaves stream == floor (freq' * duration')

                _ ->
                    False
        Nothing ->
            False
        where
            freq' = 1 + freq
            duration' = 1 + duration

-- Sine Wave Tests end

musicStreamSpec :: IO ()
musicStreamSpec = hspec $
    describe "simpleSineWave" $ do
        it "should fail when number of channels is not 1 or 2" $ do
            simpleSineWave undefined undefined undefined 0 `shouldBe` (Nothing :: Maybe Channel)
            simpleSineWave undefined undefined undefined 3 `shouldBe` (Nothing :: Maybe Channel)

        it "should return a channel of proper length" $ property prop_length_sineWave

        it "should return a channel with proper number of waves" $ property prop_numberOfWaves_sineWave
