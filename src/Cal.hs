{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Cal where

import Data.Aeson (FromJSON, ToJSON)
import Data.Function (on)
import Data.Maybe (fromJust)
import GHC.Generics
import Control.Lens (view)
import Control.Lens.TH (makeLensesWith)

import TH
    
type Year  = Int
type Month = Int
type Day   = Int

data Date = Date { year :: Year, month :: Month, day :: Day } deriving (Show, Eq, Generic)

data Period = Period { startDate :: Date, endDate :: Date } deriving (Show, Eq, Generic)

data Seasonal = Seasonal { yearOfSeason :: Year, season :: Month } deriving (Show, Eq, Generic)

instance FromJSON Date
instance FromJSON Period
instance FromJSON Seasonal

instance ToJSON Date
instance ToJSON Period
instance ToJSON Seasonal

makeLensesWith myRules ''Date
makeLensesWith myRules ''Period
makeLensesWith myRules ''Seasonal

