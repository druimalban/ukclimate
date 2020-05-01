{- file src/Sync.hs

Basic functions to build our synchronisation client from.

The workflow is as follows:

1. If it is a new day (or we are forcing a fetch), fetch the file.
   This avoids fetching data that has likely not changed.
2. Build a list of files that have changed, a simple MD5 sum is
   suitable for this as we are fetching data without authentication
   anyway.
3. Produce JSON and/or CSV as appropriate for these.
4. Report back what has failed.

-}
{-# LANGUAGE LambdaCase #-}

module Sync where

import Control.Arrow ((&&&))
import Control.Exception (IOException, catch, try)
import Control.Lens (_1, _2, over)
import Data.Aeson (decodeFileStrict, encodeFile)
import Data.Attoparsec.Text (parseOnly)
import Data.ByteString.Lazy (ByteString, readFile, writeFile)
import Data.ByteString.Lazy.Char8 (pack, toStrict)
import Data.Csv
import Data.Digest.Pure.MD5 (MD5Digest, md5)
import Data.Int (Int64)
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding.Error (UnicodeException)
import Data.Tuple.Sequence (sequenceT)
import Data.Time.Format.ISO8601
import Data.Time.LocalTime (getZonedTime, localDay, zonedTimeToLocalTime)
import Data.Vector (Vector, empty, filterM, foldr, map, mapM)
import qualified Data.Vector as V
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Prelude hiding (filterM, foldr, map, mapM, readFile, writeFile)
import Text.Read (readMaybe)
import System.Directory (createDirectoryIfMissing, doesPathExist, doesFileExist)

import CSV
import Parse
import Types

fetchTargets :: Vector Target -> FilePath -> IO (Vector (Either HttpException (IntermediateFile, Target)))
fetchTargets targets rawFilesDir = do
  timeStamp <- iso8601Show . localDay . zonedTimeToLocalTime <$> getZonedTime
  createDirectoryIfMissing True (rawFilesDir ++ "/" ++ timeStamp)
  manager <- newManager tlsManagerSettings
  let nominalTargets = map (id &&& rawFPTime timeStamp &&& rawFP) targets
  q <- filterM checkTarget nominalTargets
  mapM (getFile manager) q

  where
    -- Check TODAY's file exists or not. Not concerned with previous days' file paths.
    checkTarget :: (Target, (FilePath, FilePath)) -> IO Bool
    checkTarget (_, (cachedTargetFP, _)) = not <$> doesFileExist cachedTargetFP

    -- This is the 'current' output which will be written in the timestamped directory. Something like ~/.ukclimate/raw/$TIMESTAMP/$FILE.
    -- This is what we check if it exists before re-downloading (or if forced.)
    rawFPTime :: String -> Target -> FilePath
    rawFPTime timeStamp target = rawFilesDir ++ "/" ++ timeStamp ++ "/" ++ (targetName target) ++ "data.txt"

    -- This is the 'final' output, written after we've downloaded it. Something like ~/.ukclimate/raw/$FILE
    -- The only reason I have this is to preserve the full path.. this is a bit awkward and I don't like it.
    rawFP :: Target -> FilePath
    rawFP target = rawFilesDir ++ "/" ++ (targetName target) ++ "data.txt"

    getFile :: Manager -> (Target, (FilePath, FilePath)) -> IO (Either HttpException (IntermediateFile, Target))
    getFile m (target, fps) =
      do let url = targetURL target
         request <- parseUrlThrow url
         attemptResponse <- try (httpLbs request m)
         case attemptResponse of
           Left exreq -> return $ Left exreq
           Right intf -> return $ Right ((responseBody intf, fps), target)

-- IntermediateFile :: (ByteString, (FilePath, FilePath))
getChangedFiles :: Vector (IntermediateFile, Target) -> IO (Vector (IntermediateFile, Target))
getChangedFiles = filterM hasChanged
  -- 1. We take MD5 sums of the downloaded bytestrings held in each IntermediateFile
  -- 2. We take MD5 sums of the LAST result, that is those in ~/.ukclimate/data/raw/$FILE
  -- 3. If differing, we produce canditate files to be parsed.
hasChanged :: (IntermediateFile, Target) -> IO Bool
hasChanged ((bs, (newFP, origFP)), target) = do
    let newMD5 = md5 bs
    origExists <- doesFileExist origFP 
    if (not origExists) then return True else do
      origMD5 <- md5 <$> readFile origFP
      return (origMD5 /= newMD5)

cacheCandidates :: Vector (IntermediateFile, Target) -> IO (Vector (Either (Maybe IOException, Maybe IOException) (ByteString, Target)))
cacheCandidates v = getChangedFiles v >>= mapM cc where
  cc :: (IntermediateFile, Target) -> IO (Either (Maybe IOException, Maybe IOException) (ByteString, Target))
  cc ((b, (currFP, resFP)), target) = do
    curr <- try (writeFile currFP b) :: IO (Either IOException ())
    res  <- try (writeFile resFP  b) :: IO (Either IOException ())
    case (curr, res) of
      --(Left e, _) -> return Nothing
      --(_, Left e) -> return Nothing
      --_           -> return (Just (b, target))
      (Left  e, Left e')  -> return (Left (Just e,  Just e')) -- error for both caching operations
      (_,       Left e')  -> return (Left (Nothing, Just e')) -- error for caching final file path
      (Left  e, _)        -> return (Left (Just e,  Nothing)) -- error for caching dated file path
      (Right e, Right e') -> return (Right (b, target))        -- No errors at all!
    
decodeCandidates :: Vector (ByteString, Target) -> Vector (Either (UnicodeException) (Text, Target))
decodeCandidates = map dc where
  dc :: (ByteString, Target) -> Either (UnicodeException) (Text, Target)
  dc (b, target) =
    case (decodeUtf8' . toStrict) b of
      Left  q -> Left q
      Right txt -> Right (txt, target)

parseSites :: Vector (Text, Target) -> Vector (Either (String, String) (Site, [FlatEntry]))
parseSites = map ph where
  ph (txt, target) =
    case (parseOnly (parseHistorical target)) txt of
      Left err -> Left ("Failed parse for " ++ targetName target, err)
      Right s -> Right s

-- likewise as above for timestamp
serialiseSites :: FilePath -> Vector (Site, [FlatEntry]) -> IO (Vector (Either IOException FilePath), Vector (Either IOException FilePath))
serialiseSites outFilesDir sites = separateStreams <$> (mapM (ser outFilesDir) sites) where
  ser :: FilePath -> (Site, [FlatEntry]) -> IO (Either IOException FilePath, Either IOException FilePath)
  ser fp (s, flats) = do
    let jsonFP = fp ++ "/" ++ (siteName s) ++ ".json"
    let csvFP  = fp ++ "/" ++ (siteName s) ++ ".csv"
    
    ejson <- try (encodeFile jsonFP s) :: IO (Either IOException ())
    ecsv  <- try (writeFile csvFP (encodeByName siteHeader flats)) :: IO (Either IOException ())      

    return (ret jsonFP ejson, ret' csvFP ecsv)

  ret :: FilePath -> (Either IOException ()) -> (Either IOException FilePath)
  ret fp = \case
    Left e  -> Left e
    Right _ -> Right fp
  ret' :: FilePath -> (Either IOException ()) -> (Either IOException FilePath)
  ret' fp = \case
    Left e  -> Left e
    Right _ -> Right fp

  separateStreams :: Vector (Either IOException FilePath, Either IOException FilePath) -> (Vector (Either IOException FilePath), Vector (Either IOException FilePath))
  separateStreams streams = do
    let jsonResults = map fst streams
    let csvResults  = map snd streams

    (jsonResults, csvResults)
