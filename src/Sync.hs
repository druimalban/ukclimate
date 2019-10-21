{- file src/Sync.hs

Basic functions to build our synchronisation client from.

The workflow is as follows:

1. Fetch all the files for this date.
2. Build a list of files that have changed.
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
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Vector (Vector, empty, filterM, foldr, map, mapM)
import qualified Data.Vector as V
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Prelude hiding (filterM, foldr, map, mapM, readFile, writeFile)
import Text.Read (readMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist)

import CSV
import Parse
import Types

-- Pass TimeStamp to the function later...
fetchTargets :: Vector Target -> FilePath -> IO (Vector (Either HttpException (IntermediateFile, Target)))
fetchTargets targets rawFilesDir = do
  timeStamp <- systemSeconds <$> getSystemTime -- UTC timestamp
  createDirectoryIfMissing True (rawFilesDir ++ "/" ++ (show timeStamp))

  manager <- newManager tlsManagerSettings

  let targetsFPs = map (id &&& rawFPTime timeStamp &&& rawFP) targets

  mapM (getFile manager) targetsFPs

  where
    rawFP :: Target -> FilePath
    rawFP t = rawFilesDir ++ "/" ++ (targetName t) ++ "data.txt"
       
    rawFPTime :: Int64 -> Target -> FilePath
    rawFPTime ts t = rawFilesDir ++ "/" ++ (show ts) ++ "/" ++ (targetName t) ++ "data.txt." ++ (show ts)

    getFile :: Manager -> (Target, (FilePath, FilePath)) -> IO (Either HttpException (IntermediateFile, Target))
    getFile m (target, fps) =
      do let url = targetURL target
         request <- parseUrlThrow url
         attemptResponse <- try (httpLbs request m)
         case attemptResponse of
           Left exreq -> return $ Left exreq
           Right intf -> return $ Right ((responseBody intf, fps), target)
                                                 
getChangedFiles :: Vector (IntermediateFile, Target) -> IO (Vector (CandidateFile, Target))
getChangedFiles vf = do
  let previousFiles  = over (traverse . _1 . _2) snd vf
  collapseSums <$> filterM hasChanged (composeSums previousFiles)
  where
    composeSums :: Vector (CandidateFile, Target) -> Vector (CandidateMD5Sum, Target)
    composeSums = over (traverse . _1 . _1) (id &&& md5)
    collapseSums :: Vector (CandidateMD5Sum, Target) -> Vector (CandidateFile, Target)
    collapseSums = over (traverse . _1 . _1) (fst)

    hasChanged :: (CandidateMD5Sum, Target) -> IO Bool
    hasChanged (((recFile, newMD5), fpLast), _) = do
      origExists <- doesFileExist fpLast
      if (not origExists) then return True else do
        origMD5 <- md5 <$> readFile fpLast
        return (origMD5 /= newMD5)

cacheCandidates :: Vector (IntermediateFile, Target) -> IO (Vector (Either IOException (ByteString, Target)))
cacheCandidates v = getChangedFiles v >>= mapM cc where
  cc :: (CandidateFile, Target) -> IO (Either IOException (ByteString, Target))
  cc ((b, fp), target) = do
    res <- try (writeFile fp b) :: IO (Either IOException ())
    case res of
      Left err -> return (Left err)
      Right _  -> return (Right (b, target))

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
