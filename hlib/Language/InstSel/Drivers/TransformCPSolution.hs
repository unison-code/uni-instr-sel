--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.TransformCPSolution
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for executing transformation commands on a CP model solution.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.TransformCPSolution
  ( run )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.Drivers.DispatcherTools
  ( loadFromJson )
import Language.InstSel.ConstraintModels
import Language.InstSel.ConstraintModels.SolutionTransformer
import Language.InstSel.Utils.JSON
  ( toJson )

import Language.InstSel.Utils.IO
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
