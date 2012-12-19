module BioLab.Analysis.GrowthRate (
    minDoublingTime,
    doublingTime,
)
where
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Vector ((!))
import Statistics.Sample (Sample, mean)
import Statistics.LinearRegression (nonRandomRobustFit, defaultEstimationParameters)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)
import Statistics.Function (sortBy)
import Data.Function (on)
import Control.Arrow ((***))

newtype RawMeasurement = RawMeasurement {mVal :: Double}
newtype NormalizedMeasurement = NormalizedMeasurement {nmVal :: Double}
newtype Background = Background {bgVal :: Double}
newtype DetectionThreshold = DetectionThreshold {dtVal :: Double}
newtype LogMeasurement = LogMeasurement {lmVal :: Double}

windowSize = 5 -- minimal number of measurements needed to calculate slope

absoluteToRelativeTime :: V.Vector (UTCTime,a) -> (UTCTime, V.Vector (NominalDiffTime,a))
absoluteToRelativeTime v = (start, V.map (\(x,y) -> (x `diffUTCTime` start,y)) v)
    where
        start = fst . V.head $ v

normalize :: Background -> DetectionThreshold -> RawMeasurement -> NormalizedMeasurement
normalize (Background bg) (DetectionThreshold dt) (RawMeasurement rm)= NormalizedMeasurement . max dt $ (rm-bg)

minDoublingTime :: V.Vector (UTCTime,NormalizedMeasurement) -> NominalDiffTime
minDoublingTime = realToFrac . mean . V.map (realToFrac . snd) . V.take 3 . V.drop 2 . sortBy (compare `on` snd) . doublingTime Nothing

doublingTime :: Maybe Int -> V.Vector (UTCTime, NormalizedMeasurement) -> V.Vector (NominalDiffTime,NominalDiffTime)
doublingTime mws mes = V.fromList . map (doublingTimeWindow window_size . V.take window_size) . takeWhile (\x -> V.length x >= window_size) . iterate V.tail . V.map (realToFrac *** (logBase 2 . nmVal)) $ dmes
    where
        (start,dmes) = absoluteToRelativeTime mes
        window_size = fromMaybe windowSize mws

doublingTimeWindow :: Int -> V.Vector (Double,Double) -> (NominalDiffTime,NominalDiffTime)
doublingTimeWindow window_size v = (realToFrac . fst $ v ! (window_size `div` 2),calcDoublingTime v)

calcDoublingTime :: V.Vector (Double,Double) -> NominalDiffTime
calcDoublingTime xys = realToFrac . (1/) . snd . nonRandomRobustFit defaultEstimationParameters xs $ ys
    where
        (xs,ys) = U.unzip . U.fromList . V.toList $ xys

