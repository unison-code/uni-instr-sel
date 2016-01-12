--------------------------------------------------------------------------------
-- |
-- Module      : UniIS.Drivers.MakeCPModel
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a function graph and pattern matchset as input, and constructs a CP
-- model instance.
--
--------------------------------------------------------------------------------

module UniIS.Drivers.MakeCPModel
  ( run )
where

import UniIS.Drivers.Base
import Language.InstrSel.ConstraintModels.ModelHandler
import Language.InstrSel.Functions
  ( Function )
import Language.InstrSel.TargetMachines.Targets
  ( retrieveTargetMachine )
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

run :: MakeAction -> Function -> PatternMatchset -> IO [Output]

run MakeHighLevelCPModel function matchset =
  do let target = retrieveTargetMachine $ pmTarget matchset
     when (isNothing target) $
       reportErrorAndExit $ "Unrecognized target machine: "
                            ++ (show $ fromJust target)
     let model = mkHighLevelModel function
                                  (fromJust target)
                                  (pmMatches matchset)
     return [toOutputWithoutID $ toJson model]

run _ _ _ = reportErrorAndExit "MakeCPModel: unsupported action"
