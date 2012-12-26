module Biolab.Analysis.Types (
    Background(..),
    DetectionThreshold(..),
    RawMeasurement(..),
    NormalizedMeasurement(..),
)
where
import Biolab.Types (RawMeasurement(..))

newtype Background = Background {bgVal :: Double} deriving Show
newtype DetectionThreshold = DetectionThreshold {dtVal :: Double} deriving Show
newtype NormalizedMeasurement = NormalizedMeasurement {nmVal :: Double}
