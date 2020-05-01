{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Entries where

import Control.Lens.TH (makeLensesWith)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Csv (FromNamedRecord, ToNamedRecord, FromField,  ToField)
import Control.Exception (IOException)
import Data.Digest.Pure.MD5 (MD5Digest)
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Data.Vector (Vector, cons, empty, fromList, singleton)
import Network.HTTP.Client (HttpException)
import GHC.Generics
import GHC.Generics

import Cal
import TH

-- Data may be missing or otherwise.
data Measurement = Missing | Measured Double deriving (Show, Eq, Generic)
data Annotation = AltSensor | AltLocation | Estimated | AsIs deriving (Show, Eq, Generic)

data TargetType = Regional | Historical deriving (Show, Eq, Generic)
data RegionalFormat = Ranked | Yearly deriving (Show, Eq, Generic)

-- Possible regions corresponding to areas of Britain and Ireland ["these islands"]
data MetaRegion = ROI | UK | NI | Scotland | Wales | England | ChannelIslands | Mann | International | Britain | Ireland | TheseIslands deriving (Show, Eq, Generic)

type SingleYear = (Year, MonthlyEntry)

-- Information about a given station that we can't necessarily
-- gloss from the files downloaded. This informs how we parse.
data Target = Target {
    targetName   :: String
  , targetURL    :: String
  , targetType   :: TargetType
  , targetRegion :: MetaRegion
  } deriving (Show, Eq, Generic)

-- Site for Historical station data.
data Site = Site {
    siteName     :: String 
  , fullSiteName :: String
  , siteURL      :: FilePath 
  , siteRegion   :: MetaRegion
  , siteLoc      :: Location 
  , altSiteLoc   :: Maybe Location 
  , siteInactive :: Bool
  , siteComments :: Vector Text
  , siteEntries  :: Vector YearlyEntry }
  deriving (Show, Eq, Generic)

-- YearlyEntry, MonthlyEntry and Entry are used for JSON,
-- where we want to query by year.
data YearlyEntry = YearlyEntry {
    yearRecorded   :: Year
  , monthsRecorded :: Vector MonthlyEntry
  } deriving (Show, Eq, Generic)

data MonthlyEntry = MonthlyEntry {
    monthRecorded :: Month
  , provisional   :: Bool
  , entries       :: Vector Entry
  } deriving (Show, Eq, Generic)

data Entry = Entry {
    entryName     :: String
  , entryUnit     :: String
  , entryRecorded :: Measurement
  , entryProperty :: Annotation
  } deriving (Show, Eq, Generic)

-- Location of station as specialised in historical station data.
data Location = Location {
    easting   :: Int
  , northing  :: Int
  , irishGrid :: Bool
  , latitude  :: Maybe Double
  , longitude :: Maybe Double
  , elevation :: Int
  , period    :: Maybe Period
  } deriving (Show, Eq, Generic)
  
-- FlatEntry is used for serialising CSV.
-- It's closest to the format of the historical station data.
data FlatEntry = FlatEntry {
    yearOf       :: Year
  , monthOf      :: Month
  , maxTemp      :: Measurement
  , maxTempProp  :: Annotation
  , minTemp      :: Measurement
  , minTempProp  :: Annotation
  , airFrost     :: Measurement
  , airFrostProp :: Annotation
  , rainfall     :: Measurement
  , rainfallProp :: Annotation
  , sunshine     :: Measurement
  , sunshineProp :: Annotation
  , prov         :: Bool
  } deriving (Show, Eq, Generic)

data Region = Region {
    regionName :: String
  , metaRegion :: MetaRegion
  , yearlyEntries :: Vector RegionalEntry
  , rankedEntries :: Vector RegionalEntry
  } deriving (Show, Eq, Generic)

data RegionalEntry = RegionalEntry {
    shortStatName  :: String
  , longStatName   :: String
  , unit           :: String
  , regionalFormat :: RegionalFormat
  , url            :: FilePath
  , yearFrom       :: Year
  , comments       :: Vector Text
  , lastUpdate     :: Date
  , measurements   :: Vector Measurement
  } deriving (Show, Eq, Generic)

instance FromJSON Site
instance FromJSON MetaRegion
instance FromJSON Target
instance FromJSON TargetType
instance FromJSON YearlyEntry
instance FromJSON MonthlyEntry
instance FromJSON Entry
instance FromJSON Annotation
instance FromJSON Measurement
instance FromJSON Location
instance FromJSON Region
instance FromJSON RegionalEntry
instance FromJSON RegionalFormat

instance ToJSON Site
instance ToJSON MetaRegion
instance ToJSON Target
instance ToJSON TargetType
instance ToJSON YearlyEntry
instance ToJSON MonthlyEntry
instance ToJSON Entry
instance ToJSON Annotation
instance ToJSON Measurement
instance ToJSON Location
instance ToJSON Region
instance ToJSON RegionalEntry
instance ToJSON RegionalFormat

makeLensesWith myRules ''Location
makeLensesWith myRules ''Site
makeLensesWith myRules ''Target
makeLensesWith myRules ''YearlyEntry
makeLensesWith myRules ''MonthlyEntry
makeLensesWith myRules ''Entry
makeLensesWith myRules ''Region
makeLensesWith myRules ''RegionalEntry

data Report = Report {
    fetchFails  :: Vector HttpException                     -- URL and how it failed
  , fetchSuccs  :: Vector (IntermediateFile, Target)
  , cacheFails  :: Vector (Maybe IOException, Maybe IOException)                     -- Files fetched which failed to write
  , cacheSuccs  :: Vector (ByteString, Target)
  , decodeFails :: Vector UnicodeException                  -- Files decodes which failed
  , decodeSuccs :: Vector (Text, Target)
  , parseFails  :: Vector (String, String)                  -- Files parsed which failed
  , parseSuccs  :: Vector (Site, [FlatEntry])
  , serialFails :: (Vector IOException, Vector IOException) -- JSON *and* CSV write ailures 
  , serialSuccs :: Vector FilePath
  } deriving (Show, Generic)

type IntermediateFile   = (ByteString, (FilePath, FilePath))
type IntermediateMD5Sum = ((ByteString, MD5Digest), (FilePath, FilePath))
type CandidateFile      = (ByteString, FilePath)
type CandidateMD5Sum    = ((ByteString, MD5Digest), FilePath)

makeLensesWith myRules ''Report
