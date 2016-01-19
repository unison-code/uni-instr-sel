--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.TargetMachines.Generators.LLVM.Generator
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a LLVM-specific machine description and generates the corresponding
-- target machine.
--
--------------------------------------------------------------------------------

module Language.InstrSel.TargetMachines.Generators.LLVM.Generator where

import Language.InstrSel.TargetMachines.Generators.LLVM.Base
import Language.InstrSel.TargetMachines.Base
  ( TargetMachine (..) )
import Language.InstrSel.TargetMachines.IDs
  ( toTargetMachineID )
import Language.InstrSel.Utils
  ( capitalize )


-------------
-- Functions
-------------

generateTargetMachine :: MachineDescription -> TargetMachine
generateTargetMachine m =
  -- TODO: implement
  TargetMachine
    { tmID = toTargetMachineID $ capitalize $ mdID m
    , tmInstructions = []
    , tmLocations = []
    }
