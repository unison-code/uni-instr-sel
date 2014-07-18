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
import Data.Maybe
  ( fromJust
  , isJust
  )



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
    { -- | The ID of this instruction. The ID must be globally unique across all
      -- instructions, but not necessarily contiguous.

      instID :: InstructionID

      -- | Patterns which correspond to the instruction. There must be at least
      -- one pattern. Each pattern also has a corresponding ID which must be
      -- globally unique across all patterns and all instructions, but not
      -- necessarily contiguous.

    , instPatterns :: [InstPattern]

      -- | Instruction properties.

    , instProps :: InstProperties

      -- | Assembly string to produce upon code emission.

    , instAssemblyStr :: AssemblyString

    }
  deriving (Show)

-- | Contains the various properties of an instruction, such as code size and
-- latency.

data InstProperties =
    InstProperties
    { -- | Instruction code size (in bytes).

      instCodeSize :: Integer

      -- | Instruction latency (in cycles).

    , instLatency :: Integer

    }
  deriving (Show)

-- | Defines a pattern for a machine instruction.

data InstPattern =
    InstPattern
    { -- | The ID of this pattern. The ID must be unique within the same
      -- instruction, but not necessarily contiguous.

      patID :: PatternID

      -- | The operation structure of the pattern.

    , patOS :: OpStructure

      -- | Specifies the data nodes within the 'OpStructure' which represent
      -- output that can be observed from outside the pattern.

    , patOutputDataNodes :: [NodeID]

      -- | Indicates whether the use-def-dom constraints apply to this
      -- pattern. This will typically always be set to 'True' for all patterns
      -- except the generic phi patterns.

    , patAUDDC :: Bool

      -- | Maps an 'AssemblyID', which is denoted as the index into the list,
      -- that appear in the 'AssemblyString' of the instruction, to a particular
      -- node in the graph of the pattern's operation structure.  Because of
      -- this, all 'AssemblyID's used within the same 'AssemblyString' *must* be
      -- unique and contiguous!

    , patAssIDMaps :: [NodeID]

    }
  deriving (Show)

-- | Represents a target machine.

data TargetMachine =
    TargetMachine
    { -- | The identifier of the target machine.

      tmID :: TargetMachineID

      -- | The set of assembly instructions supported by the target machine.

    , tmInstructions :: [Instruction]

      -- | The machine registers. Each must be given a unique ID, but not
      -- necessarily in a contiguous order.

    , tmRegisters :: [( RegisterID -- ^ Register ID (only used internally).
                      , String     -- ^ Register name (needed during code
                                   -- emission).
                      )]

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
  let maybe_i = findInstruction is iid
  in if isJust maybe_i
     then let i = fromJust maybe_i
              ps = instPatterns i
              found = filter (\p -> patID p == pid) ps
          in if length found > 0
             then Just $ head found
             else Nothing
     else Nothing
