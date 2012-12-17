module BioLab.Analysis.Biomass (
    minDoublingTime,
    doublingTime,
    growthRate,
)
where
import Statistics.Sample (Sample)
import Data.Vector.Unboxed as U

newtype RawMeasurement = RawMeasurement {mVal :: Double}
newtype NormalizedMeasurement = NormalizedMeasurement {nmVal :: Double}
newtype Background = Background {bgVal :: Double}
newtype DetectionThreshold = DetectionThreshold {dtVal :: Double}
newtype LogMeasurement = LogMeasurement {lmVal :: Double}

normalize :: Background -> DetectionThreshold -> RawMeasurement -> NormalizedMeasurement
normalize (Background bg) (DetectionThreshold dt) (RawMeasurement rm)= NormalizedMeasurement . max dt $ (rm-bg)

minDoublingTime :: U.Vector (UTCTime,NormalizedMeasurement) -> NominalDiffTime

doublingTime :: U.Vector (UTCTime, NormalizedMeasurement) -> U.Vector (UTCTime,NominalDiffTime)


