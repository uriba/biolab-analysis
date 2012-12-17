module BioLab.Analysis.Biomass (
    minDoublingTime,
    doublingTime,
    growthRate,
)
where
import Statistics.Sample (Sample)
import Data.Vector.Unboxed as U
import Data.Maybe (fromMaybe)

newtype RawMeasurement = RawMeasurement {mVal :: Double}
newtype NormalizedMeasurement = NormalizedMeasurement {nmVal :: Double}
newtype Background = Background {bgVal :: Double}
newtype DetectionThreshold = DetectionThreshold {dtVal :: Double}
newtype LogMeasurement = LogMeasurement {lmVal :: Double}

normalize :: Background -> DetectionThreshold -> RawMeasurement -> NormalizedMeasurement
normalize (Background bg) (DetectionThreshold dt) (RawMeasurement rm)= NormalizedMeasurement . max dt $ (rm-bg)

minDoublingTime :: U.Vector (UTCTime,NormalizedMeasurement) -> NominalDiffTime

doublingTime (Integral a) :: Maybe a -> U.Vector (UTCTime, NormalizedMeasurement) -> U.Vector (UTCTime,NominalDiffTime)
doublingTime mws mes = map (calcDoublingTime . U.take $ window_size) . takeWhile (U.length >= window_size) . U.tails . U.map (\(x,y) -> (fromIntegral . toSeconds $ x, logBase 10 . nmVal $ y)) $ mes
    where
        window_size = fromMaybe windowSize $ mws

calcDoublingTime :: U.Vector (Double,Double) -> NominalDiffTime
calcDoublingTime xys = fromSeconds . (1/) . snd . robustFit xs $ ys
    where
        (xs,ys) = U.unzip xys
