module Biolab.Analysis.Utils (
    absoluteToRelativeTime,
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
