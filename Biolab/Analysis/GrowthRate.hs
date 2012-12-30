module Biolab.Analysis.GrowthRate (
    minDoublingTime,
    doublingTime,
)
where
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Vector ((!))
import Statistics.Sample (Sample, mean)
import Statistics.LinearRegression (nonRandomRobustFit, defaultEstimationParameters)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)
import Statistics.Function (sortBy)
import Data.Function (on)
import Control.Arrow ((***))
import Biolab.Analysis.Types
import Biolab.Types
import Biolab.Analysis.Utils

newtype LogMeasurement = LogMeasurement {lmVal :: Double}

windowSize = 5 -- minimal number of measurements needed to calculate slope

minDoublingTime :: (ColonySample a) => a NormalizedMeasurement -> Maybe NominalDiffTime
minDoublingTime = result . V.map (realToFrac . snd) . V.take 3 . V.drop 2 . sortBy (compare `on` snd) . V.map (\(x,y) -> (x,fromJust y)) . V.filter (isJust . snd) . doublingTime Nothing
    where result v = if V.null v then Nothing else Just . realToFrac . mean $ v

doublingTime :: (ColonySample a) => Maybe Int -> a NormalizedMeasurement -> V.Vector (NominalDiffTime,Maybe NominalDiffTime)
doublingTime mws mes = V.fromList . map (doublingTimeWindow window_size . V.take window_size) . takeWhile (\x -> V.length x >= window_size) . iterate V.tail . V.map (realToFrac *** (logBase 2 . nmVal)) $ dmes
    where
        (start,dmes) = absoluteToRelativeTime . measurements $ mes
        window_size = fromMaybe windowSize mws

doublingTimeWindow :: Int -> V.Vector (Double,Double) -> (NominalDiffTime,Maybe NominalDiffTime)
doublingTimeWindow window_size v = (realToFrac . fst $ v ! (window_size `div` 2),calcDoublingTime v)

calcDoublingTime :: V.Vector (Double,Double) -> Maybe NominalDiffTime
calcDoublingTime xys = fmap (realToFrac . (1/)) . gr . snd . nonRandomRobustFit defaultEstimationParameters xs $ ys
    where
        (xs,ys) = U.unzip . U.fromList . V.toList $ xys
        gr x = if x <=0 then Nothing else Just x

