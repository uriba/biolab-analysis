module Biolab.Analysis.ExpressionLevel (
    expressionLevel,
)
where

import qualified Data.Vector as V
import Data.Time (UTCTime, NominalDiffTime)
import Biolab.Types
import Biolab.Analysis.Utils

expressionLevel :: Maybe Int -> NormalizedAbsorbance -> NormalizedFluorescence -> V.Vector (NominalDiffTime, Double)
expressionLevel mws odv flv = V.zipWith (\x y -> (fst x, snd x / snd y)) fl_diff od_sum
    where
        fl_diff = derivate . mesVect $ flv
        od_sum = integrate . mesVect $ odv

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
