-- file src/Report.hs
-- Report of failures during each stage, and output to useful formats for viewing.
-- Currently outputs to text and JSON

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Report where

import Control.Exception (Exception)
import Control.Lens (_1, over, view, traverse)
import Data.Aeson (FromJSON, ToJSON, decodeFileStrict)
import Data.ByteString.Lazy (ByteString)
import Data.Either (fromLeft, fromRight, isLeft, isRight)
import Data.Either hiding (lefts, rights)
import Data.Functor.Foldable
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text (Text)
import Data.Digest.Pure.MD5 (MD5Digest)
import Data.Vector (Vector, cons, empty, filter, foldr, length, map)
import qualified Data.Vector as V ((++))
import Prelude hiding ((<$>), filter, foldr, length, map)
import Text.PrettyPrint.ANSI.Leijen hiding (empty)
import qualified Text.PrettyPrint.ANSI.Leijen as WL (empty)

import Sync
import Types

lefts :: Vector (Either a b) -> Vector a
lefts = cata alg where
  alg = \case
    NilV               -> empty
    ConsV (Left x) xs  -> x `cons` xs
    ConsV (Right _) xs -> xs

rights :: Vector (Either a b) -> Vector b
rights = cata alg where
  alg = \case
    NilV               -> empty
    ConsV (Right x) xs -> x `cons` xs
    ConsV (Left _) xs  -> xs
    
-- load targets -> fetch targets -> decode targets -> parse targets -> serialise targets -> generate report 
runReport :: FilePath -> FilePath -> FilePath -> IO (Either FilePath Report)
runReport targetsF rawFilesDir outFilesDir = do
  t <- decodeFileStrict targetsF
  case t of
    Nothing -> return (Left targetsF)
    Just targets -> do
      ft <- fetchTargets targets rawFilesDir
      let fetchFails  = lefts ft
      let fetchSuccs  = rights ft

      -- cacheCandidates :: Vector (IntermediateFile, Target) -> IO (Vector (Either (Maybe IOException, Maybe IOException) (ByteString, Target)))
      cached <- cacheCandidates fetchSuccs
      --let cacheFails  = lefts cached
      --let cacheSuccs  = rights cached
      let cacheFails  = lefts cached
      let cacheSuccs  = rights cached 
      let decoded     = decodeCandidates cacheSuccs 
      let decodeFails = lefts decoded
      let decodeSuccs = rights decoded

      let parsed      = parseSites decodeSuccs -- raw dir implicit, we must specify outdir though
      let parseFails  = lefts parsed
      let parseSuccs  = rights parsed

      -- IO actions for writing parsed data, report back success/fail as appropriate.
      serialise <- serialiseSites outFilesDir parseSuccs
      let jsonResults = fst serialise
      let csvResults  = snd serialise
      
      let serialFails = (lefts jsonResults, lefts csvResults)
      let serialSuccs = (rights jsonResults) V.++ (rights csvResults)
      
      return (Right (Report {..}))

bar = text " | " :: Doc
  
basicReport :: Report -> Doc
basicReport Report {..} = text "Step      | P | F" <> line
  <> text "Fetch    " <> bar <> text (show $ length fetchSuccs) <> bar <> text (show $ length fetchFails) <> line
  <> text "Decode   " <> bar <> text (show $ length decodeSuccs) <> bar <> text (show $ length decodeFails) <> line
  <> text "Parse    " <> bar <> text (show $ length parseSuccs)  <> bar <> text (show $ length parseFails) <> line
  <> text "Serialise" <> bar <> text (show $ length serialSuccs) <> bar <> text (show $ (length $ fst serialFails) + (length $ snd serialFails)) <> line
  
fullReport :: Report -> Doc
fullReport Report {..} = text ("Fetch: PASSES - "    ++ (show $ length fetchSuccs))
                    <$$> text ("Fetch: FAILURES - "  ++ (show $ length fetchFails))
                    <$>  indent' fetchFails
                    <>   text ("Decode: PASSES - "   ++ (show $ length decodeSuccs))
                    <$$> text ("Decode: FAILURES - " ++ (show $ length decodeFails))
                    <$>  indent' decodeFails
                    <>   text ("Parse: PASSES - "   ++ (show $ length parseSuccs))
                    <$$> text ("Parse: FAILURES - " ++ (show $ length parseFails))
                    <$>  indent' parseFails
                    <>   text ("Serialise: PASSES - "   ++ (show $ length serialSuccs))
                    <$$> text ("Serialise: FAILURES - " ++ (show $ (length $ fst serialFails) + (length $ snd serialFails)))
                    <$>  indent' (fst serialFails)
                    <>   indent' (snd serialFails)

                      where indent' :: Show a => Vector a -> Doc
                            indent' v = cata alg v where
                              alg = \case
                                NilV -> WL.empty
                                ConsV e xs -> indent 4 (text (show e)) <$> xs
