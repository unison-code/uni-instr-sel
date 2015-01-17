--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.MakeArrayIndexMapInfo
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a function graph and matchset information as input, and computes the
-- corresponding array index mapping information.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.MakeArrayIndexMapInfo
  ( run )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.ConstraintModels.ArrayIndexMapMaker
  ( mkArrayIndexMapInfo )
import Language.InstSel.Functions
  ( Function )
import Language.InstSel.TargetMachines.PatternMatching
  ( MatchsetInfo )
import Language.InstSel.Utils.JSON

import Language.InstSel.Utils.IO
  ( reportError )



-------------
-- Functions
-------------

run :: MakeAction -> Function -> MatchsetInfo -> IO [Output]

run MakeArrayIndexMapInfo function target =
  do let maps = mkArrayIndexMapInfo function target
     return [toOutputWithoutID $ toJson maps]

run _ _ _ = reportError "MakeMatchset: unsupported action"
