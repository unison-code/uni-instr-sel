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
  ( loadFromJson )
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
  -> ArrayIndexMaplists
  -> IO [Output]

run RaiseLowLevelCPSolution str ai_maps =
  do sol <- loadFromJson str
     let new_sol = raiseLowLevelSolution sol ai_maps
     return [toOutputWithoutID $ toJson new_sol]

run _ _ _ = reportError "TransformCPSolution: unsupported action"
