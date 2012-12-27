module Biolab.Analysis.Utils (
    absoluteToRelativeTime,
    )
where

import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)
import qualified Data.Vector as V

absoluteToRelativeTime :: V.Vector (UTCTime,a) -> (UTCTime, V.Vector (NominalDiffTime,a))
absoluteToRelativeTime v = (start, V.map (\(x,y) -> (x `diffUTCTime` start,y)) v)
    where
        start = fst . V.head $ v

