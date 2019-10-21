-- file src/Parse.hs
-- Use attoparsec to parse the convoluted data available from the Met Office

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parse where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Lens (_head, over, set, view)
import Data.Attoparsec.Text
import Data.Function (on)
import Data.Functor.Foldable (histo)
import Data.Text (Text, empty, pack, unpack)
import Data.Tuple.Sequence (sequenceT)
import Data.Vector (Vector, cons, fromList, map, singleton)
import qualified Data.Vector as V
import Prelude hiding (lines, map, putStrLn)
import Statistics.Function (sortBy)

import Types

-- We return both Site and FlatEntry, since these correspond to
-- data structures we need in order to serialise, at the minimum,
-- JSON and CSV respectively.
-- This doesn't involve extra work, since we retrieve FlatEntry
-- values at the minimum before we transform them as appropriate.
--parseHistorical :: Parser (String, Location, Maybe Location, Vector Text, Vector FlatEntry, Bool)

parseHistorical :: Target -> Parser (Site, [FlatEntry])
parseHistorical t = do
  fullSiteName <- unpack <$> takeTill isEndOfLine
  endOfLine
  siteLoc <- parseLoc
  endOfLine
  (altSiteLoc, siteComments) <- parseComments
  endOfLine
  skipWhile (not . isEndOfLine)
  endOfLine
  skipWhile (not . isEndOfLine)
  endOfLine
  flatItems <- many $ parseEntryH <* endOfLine
  siteInactive <- parseClosure
  
  let items = map flatEntryTo (fromList flatItems)
  let siteName    = targetName   t
  let siteURL     = targetURL    t
  let siteRegion  = targetRegion t
  let siteEntries = accumulateEntries items

  return (Site {..}, flatItems)

parseRegional :: Parser Region
parseRegional = undefined

parseClosure :: Parser Bool
parseClosure = do
  anything <- peekChar
  case anything of
    Nothing -> return False
    Just 'S' -> do q <- takeTill isEndOfLine                
                   if q == "Site closed" then
                     return True
                     else return False
    Just _ -> skipWhile (not . isEndOfLine) >> return False

parseLoc :: Parser Location
parseLoc = do
  (string "Location") <|> string ("&")
  optional (char ':')
  skipSpace
  easting <- decimal
  string "E "
  northing <- decimal
  char 'N'
  irishGrid <- parseIrishGrid
  skipSpace
  (latitude, longitude) <- parseLatLon 
  elevation <- decimal
  (string "m ") <|> (string " metres ")
  string "amsl"
  period <- optional parsePeriod
  return Location {..}

parseComments :: Parser (Maybe Location, Vector Text)
parseComments = do
  ahead <- peekChar
  case ahead of
    Just '&' -> do
      altLoc <- Just <$> parseLoc
      endOfLine
      c1 <- takeTill isEndOfLine
      endOfLine
      c2 <- takeTill isEndOfLine
      endOfLine
      c3 <- takeTill isEndOfLine
      return (altLoc, fromList [c1, c2, c3])
    Just _ -> do
      c1 <- takeTill isEndOfLine
      endOfLine
      c2 <- takeTill isEndOfLine
      endOfLine
      c3 <- takeTill isEndOfLine
      return (Nothing, fromList [c1, c2, c3])
    Nothing -> return (Nothing, V.empty)

parsePeriod :: Parser Period
parsePeriod = do
  skipSpace
  char '('
  yf <- decimal
  skipSpace
  string "to"
  skipSpace
  yt <- decimal
  char ')'
  return (Period (Date yf 1 1) (Date yt 1 1))

parseElevation :: Parser Int
parseElevation = do
  amsl <- decimal
  (string "m") <|> (string " metres")
  skipSpace
  string "amsl"
  return amsl

parseIrishGrid :: Parser Bool
parseIrishGrid = do
  ahead <- peekChar
  case ahead of
    Just ',' -> char ',' >> return False
    Just ' ' -> string " (Irish Grid),"  >> return True
    Just _ -> return False
    Nothing -> return False
    
parseLatLon :: Parser (Maybe Double, Maybe Double)
parseLatLon = do
  lat <- optional parseLat
  lon <- optional parseLon
  return (lat, lon)

parseLat :: Parser Double
parseLat = do
  string "Lat"
  skipSpace
  lat <- double
  skipSpace
  return lat

parseLon :: Parser Double
parseLon = do
  string "Lon"
  skipSpace
  lon <- double
  char ','
  skipSpace
  return lon

parseEntryH :: Parser FlatEntry
parseEntryH = do
  skipSpace
  yearOf         <- decimal
  skipSpace
  monthOf        <- decimal
  skipSpace
  maxTemp        <- parseMea
  maxTempProp    <- parseAnno
  skipSpace
  minTemp        <- parseMea
  minTempProp    <- parseAnno
  skipSpace
  airFrost       <- parseMea
  airFrostProp   <- parseAnno
  skipSpace
  rainfall       <- parseMea
  rainfallProp <- parseAnno
  skipSpace
  sunshine   <- parseMea
  (sunshineProp, prov) <- endOfEntry
  return FlatEntry {..}

endOfEntry :: Parser (Annotation, Bool)
endOfEntry = do
  anything <- peekChar
  case anything of
    Nothing -> return (AsIs, False)
    Just a -> if (isEndOfLine a) then
                return (AsIs, False)
              else
                do sunp <- parseAnno
                   afterSunP <- peekChar
                   case afterSunP of
                     Nothing -> return (sunp, False)
                     Just n -> if (isEndOfLine n) then
                                 return (sunp, False)
                               else do
                       skipSpace
                       pr <- parseProv
                       return (sunp, pr)

parseAnno :: Parser Annotation
parseAnno = (char '#' >> return AltSensor)
        <|> (char '$' >> return AltLocation)
        <|> (char '*' >> return Estimated)
        <|> (space >> return AsIs)

parseProv :: Parser Bool
parseProv = (string "Provisional" >> return True)
        <|> (endOfLine >> return False)
            
parseMea :: Parser Measurement
parseMea = (string "---" >> return Missing)
       <|> do m <- double
              return (Measured m)

accumulateEntries :: Vector (Year, MonthlyEntry) -> Vector YearlyEntry
accumulateEntries xs = do
  let sorted = sortBy (compare `on` fst) xs
  let result = histo alg sorted
  over (traverse . _monthsRecorded) sortByMonths result
  where
    alg = \case
      NilV -> V.empty
      ConsV (y,m) (anno :< NilV) -> singleton $ YearlyEntry y (singleton m)
      ConsV (y,m) (anno :< ConsV (y',_) (anno' :< _)) -> if (y == y') then
                                                           over (_head . _monthsRecorded) ((cons) m) anno
                                                         else
                                                           YearlyEntry y (singleton m) `cons` anno

    sortByMonths :: Vector MonthlyEntry -> Vector MonthlyEntry
    sortByMonths = sortBy (compare `on` view _monthRecorded)

flatEntryTo :: FlatEntry -> SingleYear
flatEntryTo (FlatEntry y m tmax tmaxp tmin tminp af afp rain rainp sun sunp prov)
  = (y, MonthlyEntry { monthRecorded = m,
                       provisional = prov,
                       entries = fromList [ Entry "tmax" "degC"  tmax tmaxp
                                          , Entry "tmin" "degC"  tmin tminp
                                          , Entry "af"   "days"  af   afp
                                          , Entry "rain" "mm"    rain rainp
                                          , Entry "sun"  "hours" sun  sunp ] } )

singleEntry :: SingleYear -> YearlyEntry
singleEntry (y, m) = YearlyEntry y (singleton m)
