{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>
  Roberto Castaneda Lozano <rcas@sics.se>

-}

module UniIS.Drivers.TransformCPSolution
  ( run )
where

import UniIS.Drivers.Base
import UniIS.Drivers.DispatcherTools
  ( loadFromJson
  , loadTargetMachine)
import Language.InstrSel.ConstraintModels
import Language.InstrSel.ConstraintModels.SolutionHandler
import Language.InstrSel.Utils.JSON
  ( toJson )

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )



-------------
-- Functions
-------------

run
  :: TransformAction
  -> String
     -- ^ The content of the (low-level or high-level) CP solution file.
  -> String
     -- ^ The content of the high-level CP model file.
  -> ArrayIndexMaplists
  -> IO [Output]

run RaiseLowLevelCPSolution str mstr ai_maps =
  do sol <- loadFromJson str
     model <- loadFromJson mstr
     target <- loadTargetMachine $ hlMachineID $ hlMachineParams model
     let new_sol = raiseLowLevelSolution sol model target ai_maps
     return [toOutput $ toJson new_sol]

run _ _ _ _ = reportErrorAndExit "TransformCPSolution: unsupported action"
