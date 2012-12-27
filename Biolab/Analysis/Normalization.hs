module Biolab.Analysis.Normalization (
    normalize,
    backgroundFromBlank,
    backgroundFromBlanks,
    thresholdFromBlank,
    thresholdFromBlanks,
)
where

import Biolab.Types (RawMeasurement)
import Biolab.Analysis.Types
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Sample as S
import Data.List (sort)
import qualified Statistics.Function as SS
import Data.Time (UTCTime)

backgroundFromBlank :: V.Vector (UTCTime,RawMeasurement) -> Background
backgroundFromBlank v = Background . S.mean . V.take (n `div` 2) . SS.sort . V.map (mVal . snd) $ v
    where
        n = V.length v

backgroundFromBlanks = Background . S.mean . V.fromList . map (bgVal . backgroundFromBlank)

thresholdBuffer = 3

thresholdFromBlank :: V.Vector (UTCTime,RawMeasurement) -> DetectionThreshold
thresholdFromBlank = DetectionThreshold . (thresholdBuffer *) . S.stdDev . U.fromList . map (mVal . snd) . V.toList

thresholdFromBlanks = DetectionThreshold . S.mean . V.fromList . map (dtVal . thresholdFromBlank)

initSize = 3

normalizeFromInit :: DetectionThreshold ->  V.Vector (UTCTime, RawMeasurement) -> V.Vector (UTCTime, NormalizedMeasurement)
normalizeFromInit t v = normalize (bg_from_init) t v
    where
        bg_from_init = Background . S.mean . V.take initSize . SS.sort . V.map (mVal . snd) $ v

knownBackground :: Double -> Background
knownBackground x = Background x

normalizeMeasurement :: Background -> DetectionThreshold -> RawMeasurement -> NormalizedMeasurement
normalizeMeasurement (Background bg) (DetectionThreshold dt) (RawMeasurement rm)= NormalizedMeasurement . max dt $ (rm-bg)

normalize :: Background -> DetectionThreshold -> V.Vector (UTCTime,RawMeasurement) -> V.Vector (UTCTime, NormalizedMeasurement)
normalize bg t = V.map (\(x,y) -> (x, normalizeMeasurement bg t y))

