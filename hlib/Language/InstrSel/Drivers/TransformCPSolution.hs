--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Drivers.TransformCPSolution
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for executing transformation commands on a CP model solution.
--
--------------------------------------------------------------------------------

module Language.InstrSel.Drivers.TransformCPSolution
  ( run )
where

import Language.InstrSel.Drivers.Base
import Language.InstrSel.Drivers.DispatcherTools
  ( loadFromJson
  , loadTargetMachine)
import Language.InstrSel.ConstraintModels
import Language.InstrSel.ConstraintModels.SolutionHandler
import Language.InstrSel.Utils.JSON
  ( toJson )

import Language.InstrSel.Utils.IO
  ( reportError )



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
     return [toOutputWithoutID $ toJson new_sol]

run _ _ _ _ = reportError "TransformCPSolution: unsupported action"
