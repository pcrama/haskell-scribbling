module Lib.SimpleReading
  ( SimpleReading(..)
  , ReadingMeter(..)
  , loadReadings
  ) where

import           Data.Aeson (ToJSON)
import           Data.Time (Day, fromGregorian)
import           GHC.Generics

data ReadingMeter = Pv2013 | Pv2022 | ElectricConsumption | GazConsumption | WaterConsumption deriving (Generic, Show, Eq, Bounded, Enum, Read)

instance ToJSON ReadingMeter

-- instance FromJSON ReadingMeter

data SimpleReading = SimpleReading
  { readingTime :: Day
  , readingMeter :: ReadingMeter
  , readingValue :: Float
  } deriving Generic

instance ToJSON SimpleReading

-- TODO: DB access?
loadReadings :: Monad m => m [SimpleReading]
loadReadings = return [
  mk 2023 5 1 Pv2022 1234.5
  , mk 2023 5 1 Pv2013 5432.1
  , mk 2023 4 25 Pv2022 1230.0
  , mk 2023 5 1 ElectricConsumption 7879.8
  , mk 2023 4 1 ElectricConsumption 7531.9
  , mk 2023 5 1 GazConsumption 27228.0
  ]
  where mk yyyy mm dd = SimpleReading (fromGregorian yyyy mm dd)
