module Biolab.Analysis.ExpressionLevel (
    expressionLevel,
)
where

import qualified Data.Vector as V
import Data.Time (UTCTime, NominalDiffTime)
import Biolab.Types
import Biolab.Analysis.Utils

expressionLevel :: Maybe Int -> NormalizedAbsorbance -> NormalizedFluorescence -> V.Vector (NominalDiffTime, Double)
expressionLevel mws odv flv = snd . absoluteToRelativeTime $ fl_diff
    where
        fl_diff = derivate . V.map (\(x,y) -> (x, nmVal y)) . measurements $ flv

derivate :: (Num b) => V.Vector (a,b) -> V.Vector (a,b)
derivate v | V.length v < 2 = error "unable to derivate"
           | otherwise = V.fromList . map head_diff . takeWhile ((2>=) . V.length) . iterate V.tail $ v
    where
        head_diff v = (fst . V.head $ v, (snd . V.head . V.tail $ v) - (snd . V.head $ v)) 
