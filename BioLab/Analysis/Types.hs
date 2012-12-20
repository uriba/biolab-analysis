module BioLab.Analysis.Types (
    Background(..),
    DetectionThreshold(..),
    RawMeasurement(..),
    NormalizedMeasurement(..),
)
where

newtype RawMeasurement = RawMeasurement {mVal :: Double}
newtype Background = Background {bgVal :: Double}
newtype DetectionThreshold = DetectionThreshold {dtVal :: Double}
newtype NormalizedMeasurement = NormalizedMeasurement {nmVal :: Double}
