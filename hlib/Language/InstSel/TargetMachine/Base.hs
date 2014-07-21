--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstSel.TargetMachine.Base
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

module Language.InstSel.TargetMachine.Base
  ( AssemblyPart (..)
  , AssemblyString (..)
  , Instruction (..)
  , InstPattern (..)
  , InstProperties (..)
  , TargetMachine (..)
  , findInstruction
  , findInstPattern
  )
where

import Language.InstSel.Graphs.IDs
  (NodeID)
import Language.InstSel.OpStructures
import Language.InstSel.Patterns.IDs
  (PatternID)
import Language.InstSel.TargetMachine.IDs
import Control.Monad
  (when)



--------------
-- Data types
--------------

-- | Represents parts of the assembly string. All 'AssemblyID's used within the
-- same 'AssemblyString' *must* be unique and contiguous!
data AssemblyPart =
    -- | Denotes string which is meant to be output verbatim.
    AssemblyVerbatim String

    -- | Denotes an immediate value.
  | AssemblyImmValue AssemblyID

    -- | Denotes a register.
  | AssemblyRegister AssemblyID

    -- | Denotes a basic block label.
  | AssemblyBBLabel AssemblyID
  deriving (Show)

-- | Record for containing the assembly string to produce during code emission.
data AssemblyString =
    AssemblyString { assStrParts :: [AssemblyPart] }
  deriving (Show)

-- | Defines a machine instruction.
data Instruction =
    Instruction
    { instID :: InstructionID
      -- ^ The ID of this instruction. The ID must be globally unique across all
      -- instructions, but not necessarily contiguous.

    , instPatterns :: [InstPattern]
      -- ^ Patterns which correspond to the instruction. There must be at least
      -- one pattern. Each pattern also has a corresponding ID which must be
      -- globally unique across all patterns and all instructions, but not
      -- necessarily contiguous.

    , instProps :: InstProperties
      -- ^ Instruction properties.

    , instAssemblyStr :: AssemblyString
      -- ^ Assembly string to produce upon code emission.
    }
  deriving (Show)

-- | Contains the various properties of an instruction, such as code size and
-- latency.

data InstProperties =
    InstProperties
    { instCodeSize :: Integer
      -- ^ Instruction code size (in bytes).

    , instLatency :: Integer
      -- ^ Instruction latency (in cycles).
    }
  deriving (Show)

-- | Defines a pattern for a machine instruction.

data InstPattern =
    InstPattern
    { patID :: PatternID
      -- ^ The ID of this pattern. The ID must be unique within the same
      -- instruction, but not necessarily contiguous.

    , patOS :: OpStructure
      -- ^ The operation structure of the pattern.

    , patOutputDataNodes :: [NodeID]
      -- ^ Specifies the data nodes within the 'OpStructure' which represent
      -- output that can be observed from outside the pattern.

    , patAUDDC :: Bool
      -- ^ Indicates whether the use-def-dom constraints apply to this
      -- pattern. This will typically always be set to 'True' for all patterns
      -- except the generic phi patterns.

    , patAssIDMaps :: [NodeID]
      -- ^ Maps an 'AssemblyID', which is denoted as the index into the list,
      -- that appear in the 'AssemblyString' of the instruction, to a particular
      -- node in the graph of the pattern's operation structure.  Because of
      -- this, all 'AssemblyID's used within the same 'AssemblyString' *must* be
      -- unique and contiguous!
    }
  deriving (Show)

-- | Represents a target machine.

data TargetMachine =
    TargetMachine
    { tmID :: TargetMachineID
      -- ^ The identifier of the target machine.

    , tmInstructions :: [Instruction]
      -- ^ The set of assembly instructions supported by the target machine.

    , tmRegisters :: [(RegisterID, RegisterName)]
      -- ^ The machine registers, given as pairs of register IDs and register
      -- names (which are needed during instruction emission). Each must be
      -- given a unique register ID, but not necessarily in a contiguous order.
    }
  deriving (Show)



-------------
-- Functions
-------------

-- | Given a list of instructions, the function finds the 'Instruction' entity
-- with matching instruction ID. If there is more than one match, the first
-- found is returned. If no such entity is found, 'Nothing' is returned.
findInstruction ::
     [Instruction]
  -> InstructionID
  -> Maybe Instruction
findInstruction is iid =
  let found = filter (\i -> instID i == iid) is
  in if length found > 0
     then Just $ head found
     else Nothing

-- | Given a list of instructions, the function finds the 'Instruction' entity
-- with matching instruction ID and pattern ID. If there is more than one match,
-- the first found is returned. If no such entity is found, 'Nothing' is
-- returned.
findInstPattern ::
    [Instruction]
  -> InstructionID
  -> PatternID
  -> Maybe InstPattern
findInstPattern is iid pid =
  do i <- findInstruction is iid
     let ps = instPatterns i
         found = filter (\p -> patID p == pid) ps
     when (length found == 0) Nothing
     return $ head found
