{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.MakeCPModel
  ( run )
where

import UniIS.Drivers.Base
import UniIS.Targets

import Language.InstrSel.ConstraintModels.ModelHandler
import Language.InstrSel.Functions
  ( Function )
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatchset (..) )
import Language.InstrSel.Utils.JSON

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )

import Data.Maybe
  ( fromJust
  , isNothing
  )



-------------
-- Functions
-------------

-- | Function for executing this driver.
run :: MakeAction -> Function -> PatternMatchset -> IO [Output]

run MakeHighLevelCPModel function matchset =
  do let target = retrieveTargetMachine $ pmTarget matchset
     when (isNothing target) $
       reportErrorAndExit $ "Unrecognized target machine: " ++
                            (show $ fromJust target)
     let model = mkHighLevelModel function
                                  (fromJust target)
                                  (pmMatches matchset)
     return [toOutput $ toJson model]

run _ _ _ = reportErrorAndExit "MakeCPModel: unsupported action"
