-- file src/CSV.hs
-- Instances and functions for CSV data

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSV where

import Data.Csv
import Data.Vector (fromList)

import Types

siteHeader :: Header
siteHeader = fromList [ "Year"      , "Month" 
                      , "Max Temp"  , "Property"
                      , "Min Temp"  , "Property"
                      , "Air-frost" , "Property"
                      , "Rainfall"  , "Property"
                      , "Sunshine"  , "Property"
                      , "Provisional" ]

-- Year,Month,Max Temp,Prop,Min Temp,Prop,Air-frost,Prop,Rainfall,Prop,Sunshine,Prop,Provisional
instance FromNamedRecord FlatEntry where
  parseNamedRecord m = FlatEntry
    <$> m .: "Year"
    <*> m .: "Month"
    <*> m .: "Max Temp"
    <*> m .: "Property"
    <*> m .: "Min Temp"
    <*> m .: "Property"
    <*> m .: "Air-frost"
    <*> m .: "Property"
    <*> m .: "Rainfall"
    <*> m .: "Property"
    <*> m .: "Sunshine"
    <*> m .: "Property"
    <*> m .: "Provisional"

instance ToNamedRecord FlatEntry where
  toNamedRecord FlatEntry{..} =
    namedRecord
      [ "Year"        .= yearOf
      , "Month"       .= monthOf
      , "Max Temp"    .= maxTemp
      , "Property"    .= maxTempProp
      , "Min Temp"    .= minTemp
      , "Property"    .= minTempProp
      , "Air-frost"   .= airFrost
      , "Property"    .= airFrostProp
      , "Rainfall"    .= rainfall
      , "Property"    .= rainfallProp
      , "Sunshine"    .= sunshine
      , "Property"    .= sunshineProp
      , "Provisional" .= prov ]

instance FromField Measurement where
  parseField "Missing"     = pure Missing
  parseField otherType = Measured <$> parseField otherType
instance ToField Measurement where
  toField Missing              = "Missing"
  toField (Measured otherType) = toField otherType 

instance FromField Annotation where
  parseField "Alt. sensor" = pure AltSensor
  parseField "Estimated" = pure Estimated
  parseField "Alt. location" = pure AltLocation
  parseField _   = pure AsIs
instance ToField Annotation where
  toField AltSensor   = "Alt. sensor"
  toField Estimated   = "Estimated"
  toField AltLocation = "Alt. location"
  toField AsIs        = ""

instance FromField Bool where
  parseField "True" = pure True
  parseField _      = pure False
instance ToField Bool where
  toField True  = "True"
  toField False = "False"

