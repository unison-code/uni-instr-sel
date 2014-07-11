--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.TargetMachine.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data types and records for representing a target machine.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.InstructionSelection.TargetMachine.Base
  (TargetMachine (..))
where

import Language.InstructionSelection.Patterns
  (Instruction)
import Language.InstructionSelection.TargetMachine.IDs



--------------
-- Data types
--------------

-- | Represents a target machine.

data TargetMachine
    = TargetMachine {

          -- | The identifier of the target machine.

          tmID :: TargetMachineID

          -- | The set of assembly instructions supported by the target machine.

        , tmInstructions :: [Instruction]

          -- | The machine registers. Each must be given a unique ID, but not
          -- necessarily in a contiguous order.

        , tmRegisters :: [( String     -- ^ Register name (needed during code
                                       -- emission).
                          , RegisterID -- ^ Register ID (only used internally).
                          )]

      }
    deriving (Show)
