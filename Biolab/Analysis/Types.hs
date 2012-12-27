module Biolab.Analysis.Types (
    Background(..),
    DetectionThreshold(..),
    RawMeasurement(..),
    NormalizedMeasurement(..),
)
where
import Biolab.Types (RawMeasurement(..), NormalizedMeasurement(..))

newtype Background = Background {bgVal :: Double} deriving Show
newtype DetectionThreshold = DetectionThreshold {dtVal :: Double} deriving Show
