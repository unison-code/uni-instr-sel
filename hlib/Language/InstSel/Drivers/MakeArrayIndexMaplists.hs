--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.MakeArrayIndexMaplists
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a function graph and pattern matchset as input, and computes the
-- corresponding array index mapset.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.MakeArrayIndexMaplists
  ( run )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.Drivers.DispatcherTools
  ( loadTargetMachine )
import Language.InstSel.ConstraintModels.ArrayIndexMaplistsMaker
import Language.InstSel.Functions
  ( Function )
import Language.InstSel.TargetMachines.PatternMatching
  ( PatternMatchset (..) )
import Language.InstSel.Utils.JSON

import Language.InstSel.Utils.IO
  ( reportError )



-------------
-- Functions
-------------

run :: MakeAction -> Function -> PatternMatchset -> IO [Output]

run MakeArrayIndexMaplists function matchset =
  do tm <- loadTargetMachine (pmTarget matchset)
     let mapset = mkArrayIndexMaplists function tm (pmMatches matchset)
     return [toOutputWithoutID $ toJson mapset]

run _ _ _ = reportError "MakeArrayIndexMaplists: unsupported action"
