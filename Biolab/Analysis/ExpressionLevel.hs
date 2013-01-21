module Biolab.Analysis.ExpressionLevel (
    expressionLevel,
--     segalLabExpressionLevel,
)
where

import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Vector.Unboxed as U
import Data.Time (UTCTime, NominalDiffTime)
import Biolab.Types
import Biolab.Analysis.Utils

windowSize = 4
expressionLevel :: Maybe Int -> NormalizedAbsorbance -> NormalizedFluorescence -> V.Vector (NominalDiffTime, Double)
expressionLevel mws odv flv = V.zipWith (\x y -> (fst x, snd x / snd y)) fl_diff od_sum
    where
        fl_diff_vec = map (V.fromList . U.toList) $ zipWith exponentialDerivative (map (U.fromList . V.toList . V.map realToFrac) flxs) (map (U.fromList . V.toList) flys)
        (flxs,flys) = unzip . map (V.unzip . V.take windowSize) . takeWhile ((windowSize <=) . V.length) .  iterate V.tail . mesVect $ flv
        od_sum_vec = map (V.fromList . U.toList) $ zipWith exponentialApproximation (map (U.fromList . V.toList . V.map realToFrac) odxs) (map (U.fromList . V.toList) odys)
        (odxs,odys) = unzip . map (V.unzip . V.take windowSize) . takeWhile ((windowSize <=) . V.length) .  iterate V.tail . mesVect $ odv 
        fl_diff = V.fromList . zipWith (\xs ys -> (xs ! (windowSize `div` 2), ys ! (windowSize `div` 2))) flxs $ fl_diff_vec
        od_sum = V.fromList . zipWith (\xs ys -> (xs ! (windowSize `div` 2), ys ! (windowSize `div` 2))) odxs $ od_sum_vec
        -- fl_diff = derivate . mesVect $ flv
        -- od_sum = integrate . mesVect $ odv

mesVect :: (ColonySample a) => a NormalizedMeasurement -> V.Vector (NominalDiffTime,Double)
mesVect = snd . absoluteToRelativeTime . V.map (\(x,y) -> (x, nmVal y)) . measurements

derivate :: (Num b) => V.Vector (a,b) -> V.Vector (a,b)
derivate v | V.length v <= 2 = error "unable to derivate"
           | otherwise = V.fromList . map head_diff . takeWhile ((2<=) . V.length) . iterate V.tail $ v
    where
        head_diff v = (fst . V.head $ v, (snd . V.head . V.tail $ v) - (snd . V.head $ v)) 

integrate :: (Num b) => V.Vector (a,b) -> V.Vector (a,b)
integrate v | V.length v <= 2 = error "unable to integrate"
           | otherwise = V.fromList . map head_sum . takeWhile ((2<=) . V.length) . iterate V.tail $ v
    where
        head_sum v = (fst . V.head $ v, (snd . V.head . V.tail $ v) + (snd . V.head $ v)) 
        {-
------------------------------
windowOfSize :: Int -> Double -> Series -> Series
windowOfSize ws x xs = G.take ws . G.drop data_before_x $ xs
    where
        data_before_x = (length . takeWhile ((x>) . fst) . G.toList $ xs) - (ws `div` 2)

integrate :: Series -> Double -> Double
integrate f x = sum . map snd . G.toList $ windowOfSize window_size x f

delta :: Series -> Double -> Double
delta f x = let w = windowOfSize window_size x f
            in (snd . G.last $ w) - (snd . G.head $ w)
        
window_size = 5
---------------------------------------------------
type ExpressionLevelEstimateAtConstOd = Double -> Series -> Series -> Double

check :: Series -> Series
check v = if G.length v < 4 then error "too short" else v

integralEstimation :: ExpressionLevelEstimateAtConstOd
integralEstimation target_od ods fs = logBase 2 $ 0.1 + (abs $ delta (realTime fs) mid_od_time) / integrate ods mid_od_time
    where
        mid_od_time = fst . fromMaybe default_point . find ((target_od <) . snd) . G.toList $ ods
        default_point = G.head . G.drop 3 . check $ ods

-- can be plugged into expressionLevels for interrogation.
expressionLevelByOD :: ExpressionLevelEstimateAtConstOd -> Series -> Series -> Series
expressionLevelByOD f ods fs = G.fromList . map (\(t,od) -> (od, f od real_ods fs)) . G.toList $ real_ods
    where
        real_ods = G.fromList . dropWhile ((min_od <) . snd) . takeWhile ((max_od >) . snd) . G.toList $ ods
        min_od = minimum . map snd . G.toList $ ods
        max_od = maximum . map snd . G.toList $ ods

expressionLevels :: Series -> Series -> Series
expressionLevels = expressionLevelByOD integralEstimation
-}
