--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.MakeCPModel
-- Copyright   : (c) Gabriel Hjort Blindell 2014
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

module Language.InstSel.Drivers.MakeCPModel
  ( run )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.ConstraintModels.ModelHandler
import Language.InstSel.Functions
  ( Function )
import Language.InstSel.TargetMachines.Targets
  ( retrieveTargetMachine )
import Language.InstSel.TargetMachines.PatternMatching
  ( PatternMatchset (..) )
import Language.InstSel.Utils.JSON

import Language.InstSel.Utils.IO
  ( reportError )

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
       reportError $ "Unrecognized target machine: " ++ (show $ fromJust target)
     let model = mkHighLevelModel function
                                  (fromJust target)
                                  (pmMatches matchset)
     return [toOutputWithoutID $ toJson model]

run _ _ _ = reportError "MakeCPModel: unsupported action"
