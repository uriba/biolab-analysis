module Biolab.Analysis.Normalization (
    normalize,
    normalizeFromInit,
    backgroundFromBlank,
    backgroundFromBlanks,
    thresholdFromBlank,
    thresholdFromBlanks,
)
where

import Biolab.Types
import Biolab.Analysis.Types
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Sample as S
import Data.List (sort)
import qualified Statistics.Function as SS
import Data.Time (UTCTime)

backgroundFromBlank :: (ColonySample a) => a RawMeasurement -> Background
backgroundFromBlank v = calcBG . V.take (n `div` 2) . SS.sort . V.map (mVal . snd) . measurements $ v
    where
        n = V.length . measurements $ v

backgroundFromBlanks :: (ColonySample a) => [a RawMeasurement] -> Background
backgroundFromBlanks ms
    | null ms = error "No blank values given"
    | otherwise = Background . S.mean . V.fromList . map (bgVal . backgroundFromBlank) $ ms

thresholdBuffer = 3

thresholdFromBlank :: (ColonySample a) => a RawMeasurement -> DetectionThreshold
thresholdFromBlank = calcTH . V.map (mVal . snd) . measurements

thresholdFromBlanks :: (ColonySample a) => [a RawMeasurement] -> DetectionThreshold
thresholdFromBlanks ms
    | null ms = error "No blank values given"
    | otherwise = DetectionThreshold . S.mean . V.fromList . map (dtVal . thresholdFromBlank) $ ms

initSize = 6

calcBG = Background . S.mean

calcTH = DetectionThreshold . (thresholdBuffer *) . S.stdDev

normalizeFromInit :: (ColonySample a) => a RawMeasurement-> a NormalizedMeasurement
normalizeFromInit v = normalize (bg_from_init) th_from_init v
    where
        bg_from_init = calcBG init_sample
        th_from_init = calcTH init_sample
        init_sample = V.take initSize . V.map (mVal . snd) . measurements $ v

knownBackground :: Double -> Background
knownBackground x = Background x

normalizeMeasurement :: Background -> DetectionThreshold -> RawMeasurement -> NormalizedMeasurement
normalizeMeasurement (Background bg) (DetectionThreshold dt) (RawMeasurement rm)= NormalizedMeasurement . max dt $ (rm-bg)

normalize :: (ColonySample a) => Background -> DetectionThreshold -> a RawMeasurement-> a NormalizedMeasurement
normalize bg t = process adjust_measurements
    where
        adjust_measurements = V.map (\(x,y) -> (x, normalizeMeasurement bg t y))
