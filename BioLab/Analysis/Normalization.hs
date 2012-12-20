module BioLab.Analysis.Normalization (
    normalize,
)
where

import BioLab.Analysis.Types
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Sample as S
import Statistics.Function (sort)
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)

backgroundFromBlank :: V.Vector (UTCTime,RawMeasurement) -> Background
backgroundFromBlank v = Background . S.mean . U.fromList . take (n `div` 2) . V.toList . sort . V.map (mVal . snd) $ v
    where
        n = V.length v

initSize = 3

normalizeFromInit :: DetectionThreshold -> V.Vector(UTCTime, RawMeasurement) -> V.Vector (UTCTime, NormalizedMeasurement)
normalizeFromInit t v = V.map (\(x,y) -> (x, normalize (bg_from_init) t y)) v
    where
        bg_from_init = Background . S.mean . V.take initSize . sort . V.map (mVal . snd) $ v

knownBackground :: Double -> Background
knownBackground x = Background x

normalize :: Background -> DetectionThreshold -> RawMeasurement -> NormalizedMeasurement
normalize (Background bg) (DetectionThreshold dt) (RawMeasurement rm)= NormalizedMeasurement . max dt $ (rm-bg)

