{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.TargetMachines.Generators.InstructionSynthesizer
  ( addDualTargetBranchInstructions
  , mkDualTargetBranchInstructions
  )
where

import Language.InstrSel.TargetMachines.Base
import Language.InstrSel.TargetMachines.Generators.GenericInstructions
  ( reassignInstrIDs )



-------------
-- Functions
-------------

-- | From a given target machine, constructs a new target machine that contains
-- synthesized branch instructions that can jump to both the true and the false
-- branch. These are synthesized using the branch instructions already in the
-- target machine.
addDualTargetBranchInstructions :: TargetMachine -> Either String TargetMachine
addDualTargetBranchInstructions tm =
  do let is = getAllInstructions tm
     dual_br_is <- mkDualTargetBranchInstructions is
     let new_is = reassignInstrIDs 0 (is ++ dual_br_is)
     return $ replaceAllInstructions new_is tm

-- | From a given list of instructions, constructs new branch instructions that
-- can jump to both the true and the false branch. These are synthesized using
-- the branch instructions already in the list of instructions.
mkDualTargetBranchInstructions
  :: [Instruction]
     -- ^ List of existing instructions.
  -> Either String [Instruction]
     -- ^ The synthesized instructions if successful, or an error message.
mkDualTargetBranchInstructions is = Left "blaa"
