--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstSel.TargetMachines.Targets.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- A module for retrieving specific target machine data.
--
--------------------------------------------------------------------------------

module Language.InstSel.TargetMachines.Targets.Base
  ( getTargetMachine )
where

import Language.InstSel.TargetMachines.Base
import Language.InstSel.TargetMachines.IDs

import Language.InstSel.TargetMachines.Targets.Mips32
import Language.InstSel.TargetMachines.Targets.Test



-------------
-- Functions
-------------

-- | Retrieves a specific target machine. If no machine exists with such an
-- identifier, 'Nothing' is returned.
getTargetMachine :: TargetMachineID -> Maybe TargetMachine
getTargetMachine s =
  case (fromTargetMachineID s) of
    "mips32" -> Just tmMips32
    "test"   -> Just tmTest
    _ -> Nothing
