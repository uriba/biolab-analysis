module Biolab.Analysis.Normalization (
    normalize,
    backgroundFromBlank,
)
where

import Biolab.Types (RawMeasurement)
import Biolab.Analysis.Types
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Sample as S
import Data.List (sort)
import qualified Statistics.Function as SS
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)

backgroundFromBlank :: V.Vector (UTCTime,RawMeasurement) -> Background
backgroundFromBlank v = Background . S.mean . U.fromList . take (n `div` 2) . sort . map (mVal . snd) . V.toList $ v
    where
        n = V.length v

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

