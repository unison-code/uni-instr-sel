--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Drivers.MakePatternMatchset
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a function and target machine as input, and produces the corresponding
-- pattern matchset by performing pattern matching of all instruction patterns
-- in the target machine on the function graph.
--
--------------------------------------------------------------------------------

module Language.InstrSel.Drivers.MakePatternMatchset
  ( run )
where

import Language.InstrSel.Drivers.Base
import Language.InstrSel.Functions
  ( Function )
import Language.InstrSel.TargetMachines
  ( TargetMachine )
import Language.InstrSel.TargetMachines.PatternMatching
  ( mkPatternMatchset )
import Language.InstrSel.Utils.JSON

import Language.InstrSel.Utils.IO
  ( reportError )



-------------
-- Functions
-------------

run :: MakeAction -> Function -> TargetMachine -> IO [Output]

run MakePatternMatchset function target =
  do let matches = mkPatternMatchset function target
     return [toOutputWithoutID $ toJson matches]

run _ _ _ = reportError "MakePatternMatchset: unsupported action"
