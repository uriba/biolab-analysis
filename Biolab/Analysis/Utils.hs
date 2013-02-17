module Biolab.Analysis.Utils (
    absoluteToRelativeTime,
    trim,
    grEstimationParameters,
    exponentialApproximation,
    exponentialDerivative,
    exponentialFit,
    )
where

import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)
import Statistics.Types (Sample(..))
import Statistics.LinearRegression (nonRandomRobustFit, defaultEstimationParameters, EstimationParameters(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

absoluteToRelativeTime :: V.Vector (UTCTime,a) -> (UTCTime, V.Vector (NominalDiffTime,a))
absoluteToRelativeTime v = (start, V.map (\(x,y) -> (x `diffUTCTime` start,y)) v)
    where
        start = fst . V.head $ v

trim :: (Ord a) => V.Vector (NominalDiffTime,a) -> V.Vector (NominalDiffTime,a)
trim = V.reverse . trim_prefix . V.reverse . trim_suffix . remove_initial_spike
    where
        trim_suffix v = V.takeWhile ((< (V.maximum . V.map snd $ v)) . snd) v
        trim_prefix v = V.takeWhile ((>  (V.minimum . V.map snd $ v)) . snd) v
        remove_initial_spike v = if (V.maximum . V.map snd $ v) == (V.maximum . V.map snd . V.takeWhile ((< 4800) . realToFrac . fst) $ v)
                                        then V.tail . V.dropWhile ((< (V.maximum . V.map snd $ v)) . snd) $ v
                                        else v

grEstimationParameters = defaultEstimationParameters {outlierFraction = 0.4}

exponentialApproximation :: Sample -> Sample -> Sample
exponentialApproximation xs ys = U.map (\x -> exp (beta*x+alpha)) xs
    where
        (alpha,beta) = exponentialFit xs ys

exponentialFit :: Sample -> Sample -> (Double,Double)
exponentialFit xs ys = nonRandomRobustFit grEstimationParameters xs . U.map log $ ys

exponentialDerivative :: Sample -> Sample -> Sample
exponentialDerivative xs ys = U.map (beta*) . exponentialApproximation xs $ ys
    where
        (alpha,beta) = exponentialFit xs ys
