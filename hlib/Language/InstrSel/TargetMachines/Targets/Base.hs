--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstrSel.TargetMachines.Targets.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2015
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- A module for retrieving specific target machine data.
--
--------------------------------------------------------------------------------

module Language.InstrSel.TargetMachines.Targets.Base
  ( retrieveTargetMachine )
where

import Language.InstrSel.TargetMachines.Base
import Language.InstrSel.TargetMachines.Transformations

import Language.InstrSel.TargetMachines.Targets.Mips32
import Language.InstrSel.TargetMachines.Targets.Test



-------------
-- Functions
-------------

-- | Retrieves a target machine with a specific ID. If no machine exists with
-- such an identifier, 'Nothing' is returned.
retrieveTargetMachine :: TargetMachineID -> Maybe TargetMachine
retrieveTargetMachine tmid =
  do tm <- case fromTargetMachineID tmid of
             "mips32" -> Just tmMips32
             "test"   -> Just tmTest
             _        -> Nothing
     return $ copyExtend tm
