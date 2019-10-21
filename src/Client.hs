-- file src/Client.hs
-- Our main client. This is used to fetch, syncrhonise, and convert between different file formats

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo   #-}

module Main where

import Options.Applicative
import System.Directory (getHomeDirectory)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Report
import Types

main :: IO ()
main = do
  homedir <- getHomeDirectory
  runClient =<< execParser (opts homedir)
  where
    opts h = info ((parseOpts h) <**> helper)
      ( fullDesc
     <> progDesc "Use public Met Office datasets for fun!"
     <> header "See `man ukclimate` for in-depth help" )

data Opts = Run { verbosity :: Int, targetsFP :: FilePath, rawDir :: FilePath, outDir :: FilePath, reportDir :: FilePath } 
          | ShowData { itemsToShow :: [String], showFromDir :: FilePath }
          deriving Show

parseDefault, parseOpts, parseShowData :: FilePath -> Parser Opts
parseOpts homedir = (parseShowData homedir) <|> (parseDefault homedir) <|> pure (Run 1 (homedir ++ "/.ukclimate/data/sites.json") (homedir ++ "/.ukclimate/data/raw/") (homedir ++ "/.ukclimate/data/out/") "/tmp/")

parseDefault homedir = do
  flag' Run (long "run" <> short 'r' <> help "Run all options using DIR for raw output OTHERDIR for generated output, as well as REPORTDIR for outputting the report(s)")
  verbosity <- length <$> many (flag' () (short 'v'))
  targetsFP <- strOption (long "targets" <> short 'f' <> value (homedir ++ "/.ukclimate/data/sites.json") <> metavar "FILE")
  rawDir    <- strOption (long "raw"     <> short 'd' <> value (homedir ++ "/.ukclimate/data/raw/") <> metavar "DIR")
  outDir    <- strOption (long "out"     <> short 'o' <> value (homedir ++ "/.ukclimate/data/out/") <> metavar "OTHERDIR")
  reportDir <- strOption (long "report"  <> short 'r' <> value "/tmp/" <> metavar "REPORTDIR")
  pure Run {..}

parseShowData homedir = do
  flag' ShowData (long "show" <> short 's' <> help "Print generated data for ITEM names to stdout, from DIR directory searched")
  itemsToShow <- many (argument str (metavar "ITEM"))
  showFromDir <- strOption (long "from" <> short 'd' <> value (homedir ++ "/.ukcimate/data/out/") <> metavar "DIR")
  pure ShowData {..}

runClient :: Opts -> IO ()
runClient (Run {..}) = do
  r <- runReport targetsFP rawDir outDir
  case r of
    Left fp -> putStrLn ("Failed for report " ++ fp ++ ". Is the file formatted as JSON?")
    Right report -> do
      let fr = fullReport report
      let br = basicReport report
      let reportFP = reportDir ++ "/report.txt"

      putDoc (bold $ text "Results" <> line)

      writeFile reportFP (show fr)

      if verbosity == 1 then putDoc br
        else if verbosity == 2 then putDoc fr
        else return ()
      putDoc (bold (text "Wrote full report to") <+> underline (text reportFP) <> line)
        
runClient _ = return ()
