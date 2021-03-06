module Biolab.Analysis.GrowthRate (
    minDoublingTime,
    doublingTime,
    growthRate,
)
where
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Vector ((!))
import Statistics.Sample (Sample, mean)
import Statistics.LinearRegression (linearRegression, nonRandomRobustFit, defaultEstimationParameters, EstimationParameters(..))
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)
import Statistics.Function (sortBy)
import Data.Function (on)
import Control.Arrow ((***))
import Biolab.Analysis.Types
import Biolab.Types
import Biolab.Analysis.Utils

newtype LogMeasurement = LogMeasurement {lmVal :: Double}

windowSize = 6 -- minimal number of measurements needed to calculate slope

minDoublingTime :: (ColonySample a) => a NormalizedMeasurement -> Maybe NominalDiffTime
minDoublingTime = result . V.map (realToFrac . snd) . V.take 4 . sortBy (compare `on` snd) . V.map (\(x,y) -> (x,fromJust y)) . V.filter (isJust . snd) . doublingTime Nothing
    where result v = if V.null v then Nothing else Just . realToFrac . mean $ v

doublingTime :: (ColonySample a) => Maybe Int -> a NormalizedMeasurement -> V.Vector (NominalDiffTime,Maybe NominalDiffTime)
doublingTime mws mes = V.fromList . map ((\(x,y) -> (x,fmap (realToFrac . (1/)) y)) . growthRateWindow . V.take window_size) . takeWhile (\x -> V.length x >= window_size) . iterate V.tail . V.map (realToFrac *** (logBase 2 . nmVal)) . trim $ dmes
    where
        (start,dmes) = absoluteToRelativeTime . measurements $ mes
        window_size = fromMaybe windowSize mws

growthRate :: (ColonySample a) => Maybe Int -> a NormalizedMeasurement -> V.Vector (NominalDiffTime,Maybe Double)
growthRate mws mes = V.fromList . map (growthRateWindow . V.take window_size) . takeWhile (\x -> V.length x >= window_size) . iterate V.tail . V.map (realToFrac *** (logBase 2 . nmVal)) . trim $ dmes
    where
        (start,dmes) = absoluteToRelativeTime . measurements $ mes
        window_size = fromMaybe windowSize mws

growthRateWindow :: V.Vector (Double,Double) -> (NominalDiffTime,Maybe Double)
growthRateWindow v = (realToFrac . fst $ v ! 0 {-(window_size `div` 2) -},calcGrowthRate v)

calcGrowthRate :: V.Vector (Double,Double) -> Maybe Double
calcGrowthRate xys = gr . snd . nonRandomRobustFit grEstimationParameters xs $ ys
    where
        (xs,ys) = U.unzip . U.fromList . V.toList $ xys
        gr x = if x <=0 then Nothing else Just x

