{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Lens (set)
import Control.Lens.TH (LensRules, lensField, lensRules, makeLensesWith, DefName (TopName))
import Language.Haskell.TH (mkName, nameBase)

myRules = set lensField (\_ _ n -> [TopName (mkName ("_" ++ nameBase n))]) lensRules
