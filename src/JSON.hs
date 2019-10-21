-- file src/JSON.hs
-- JSON data serialisation using Aeson

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON where

import Data.Aeson

writeJSON :: FilePath -> Site -> IO ()
writeJSON fp = encodeFile fp

readJSON :: FilePath -> IO (Maybe Site)
readJSON fp = decodeFileStrict fp
