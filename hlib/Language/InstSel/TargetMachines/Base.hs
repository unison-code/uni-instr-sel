--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstSel.TargetMachines.Base
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

module Language.InstSel.TargetMachines.Base
  ( module Language.InstSel.Graphs.IDs
  , module Language.InstSel.Patterns.IDs
  , module Language.InstSel.TargetMachines.IDs
  , AssemblyString (..)
  , AssemblyStringPart (..)
  , Instruction (..)
  , InstrPattern (..)
  , InstrProperties (..)
  , Register (..)
  , TargetMachine (..)
  , findInstruction
  , findInstrPattern
  , findRegister
  )
where

import Language.InstSel.Graphs.IDs
  ( NodeID )
import Language.InstSel.OpStructures
import Language.InstSel.Patterns.IDs
  ( PatternID )
import Language.InstSel.TargetMachines.IDs



--------------
-- Data types
--------------

-- | Record for containing the assembly string to produce during code emission.
data AssemblyString
  = AssemblyString { assemblyStrParts :: [AssemblyStringPart] }
  deriving (Show)

-- | Represents parts of the assembly string.
data AssemblyStringPart
    -- | Denotes string which is meant to be output verbatim.
  = ASVerbatim String
    -- | Denotes the immediate value of a given data node.
  | ASImmValueOfDataNode NodeID
    -- | Denotes the register allocated to a given data node.
  | ASRegisterOfDataNode NodeID
    -- | Denotes the basic block label of a given label node.
  | ASBBLabelOfLabelNode NodeID
    -- | Denotes the basic block label of the basic block in which the definer
    -- of a given data node has been placed.
  | ASBBLabelOfDataNode NodeID
  deriving (Show)

-- | Defines a machine instruction.
data Instruction
  = Instruction
      { instrID :: InstructionID
        -- ^ The ID of this instruction. The ID must be globally unique across
        -- all instructions, but not necessarily contiguous.
      , instrPatterns :: [InstrPattern]
        -- ^ Patterns which correspond to the instruction. There must be at
        -- least one pattern. Each pattern also has a corresponding ID which
        -- must be globally unique across all patterns and all instructions, but
        -- not necessarily contiguous.
      , instrProps :: InstrProperties
        -- ^ Instruction properties.
      }
  deriving (Show)

-- | Contains the various properties of an instruction, such as code size and
-- latency.
data InstrProperties
  = InstrProperties
      { instrCodeSize :: Integer
        -- ^ Instruction code size (in bytes).
      , instrLatency :: Integer
        -- ^ Instruction latency (in cycles).
      }
  deriving (Show)

-- | Defines a pattern for a machine instruction.
data InstrPattern
  = InstrPattern
      { patID :: PatternID
        -- ^ The ID of this pattern. The ID must be unique within the same
        -- instruction, but not necessarily contiguous.
      , patOS :: OpStructure
        -- ^ The operation structure of the pattern.
      , patOutputDataNodes :: [NodeID]
        -- ^ Specifies the data nodes within the 'OpStructure' which represent
        -- the output that can be observed from outside the pattern.
      , patADDUC :: Bool
        -- ^ Indicates whether the def-dom-use constraints apply to this
        -- pattern. This will typically always be set to 'True' for all patterns
        -- except the generic phi patterns.
      , patAssemblyStr :: AssemblyString
        -- ^ Assembly string to produce upon code emission if this pattern is
        -- selected.
      }
  deriving (Show)

-- | Represents a target machine.
data TargetMachine
  = TargetMachine
      { tmID :: TargetMachineID
        -- ^ The identifier of the target machine.
      , tmInstructions :: [Instruction]
        -- ^ The set of assembly instructions supported by the target machine.
      , tmRegisters :: [Register]
        -- ^ The machine registers, given as pairs of register IDs and register
        -- names (which are needed during instruction emission). Each must be
        -- given a unique register ID, but not necessarily in a contiguous
        -- order.
      }
  deriving (Show)

-- | Represents a machine register.
data Register
  = Register
      { regID :: RegisterID
        -- ^ The ID of this register. This must be unique for every register
        -- within the same target machine.
      , regName :: RegisterName
        -- ^ The name of this register (as it shall appear in the assembly
        -- string).
      }
  deriving (Show, Eq)



-------------
-- Functions
-------------

-- | Given a list of instructions, the function finds the 'Instruction' entity
-- with matching instruction ID. If there is more than one match, the first
-- found is returned. If no such entity is found, 'Nothing' is returned.
findInstruction :: [Instruction] -> InstructionID -> Maybe Instruction
findInstruction is iid =
  let found = filter (\i -> instrID i == iid) is
  in if length found > 0
     then Just $ head found
     else Nothing

-- | Given a list of instruction patterns, the function finds the 'InstrPattern'
-- entity with matching pattern ID. If there is more than one match, the first
-- found is returned. If no such entity is found, 'Nothing' is returned.
findInstrPattern ::
    [InstrPattern]
  -> PatternID
  -> Maybe InstrPattern
findInstrPattern ps pid =
  let found = filter (\p -> patID p == pid) ps
  in if length found > 0
     then Just $ head found
     else Nothing

-- | Given a list of registers, the function finds the 'Register' with matching
-- register ID. If there is more than one match, the first found is returned. If
-- no such entity is found, 'Nothing' is returned.
findRegister :: [Register] -> RegisterID -> Maybe Register
findRegister rs rid =
  let found = filter (\r -> regID r == rid) rs
  in if length found > 0
     then Just $ head found
     else Nothing
