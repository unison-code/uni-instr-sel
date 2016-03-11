--------------------------------------------------------------------------------
-- |
-- Module      : UniIS.Drivers.MakeArrayIndexMaplists
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
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

module UniIS.Drivers.MakeArrayIndexMaplists
  ( run )
where

import UniIS.Drivers.Base
import UniIS.Drivers.DispatcherTools
  ( loadTargetMachine )
import Language.InstrSel.ConstraintModels.ArrayIndexMaplistsMaker
import Language.InstrSel.Functions
  ( Function )
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatchset (..) )
import Language.InstrSel.Utils.JSON

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )



-------------
-- Functions
-------------

run :: MakeAction -> Function -> PatternMatchset -> IO [Output]

run MakeArrayIndexMaplists function matchset =
  do tm <- loadTargetMachine (pmTarget matchset)
     let mapset = mkArrayIndexMaplists function tm (pmMatches matchset)
     return [toOutput $ toJson mapset]

run _ _ _ = reportErrorAndExit "MakeArrayIndexMaplists: unsupported action"
