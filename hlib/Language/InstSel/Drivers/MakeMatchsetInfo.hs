--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.MakeMatchsetInfo
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a function and target machine as input, and produces the corresponding
-- matchset information by performing pattern matching of all instruction
-- patterns in the target machine on the function graph.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.MakeMatchsetInfo
  ( run )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.Functions
  ( Function )
import Language.InstSel.TargetMachines
  ( TargetMachine )
import Language.InstSel.TargetMachines.PatternMatching
  ( mkMatchsetInfo )
import Language.InstSel.Utils.JSON

import Language.InstSel.Utils.IO
  ( reportError )



-------------
-- Functions
-------------

run :: MakeAction -> Function -> TargetMachine -> IO [Output]

run MakeMatchsetInfo function target =
  do let matches = mkMatchsetInfo function target
     return [toOutputWithoutID $ toJson matches]

run _ _ _ = reportError "MakeMatchsetInfo: unsupported action"