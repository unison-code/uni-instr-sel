--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.Patterns.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains the data types and records for representing patterns. This is the
-- format on which subsequent preparation for instruction selection will build
-- on (i.e. other pattern forms, such as those based on LLVM, will be converted
-- into this format).
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Patterns.Base
  ( Instruction (..)
  , InstPattern (..)
  , InstProperties (..)
  )
where

import Language.InstructionSelection.Graphs (NodeID)
import Language.InstructionSelection.OpStructures
import Language.InstructionSelection.Patterns.AssemblyString
import Language.InstructionSelection.Patterns.IDs



--------------
-- Data types
--------------

-- | Defines a machine instruction.

data Instruction
    = Instruction {

          -- | The ID of this instruction. The ID must be globally unique across
          -- all instructions, but not necessarily contiguous.

          instID :: InstructionID

          -- | Patterns which correspond to the instruction. There must be at
          -- least one pattern. Each pattern also has a corresponding ID which
          -- must be globally unique across all patterns and all instructions,
          -- but not necessarily contiguous.

        , instPatterns :: [InstPattern]

          -- | Instruction properties.

        , instProps :: InstProperties

          -- | Assembly string to produce upon code emission.

        , instAssemblyStr :: AssemblyString

      }
    deriving (Show)

-- | Contains the various properties of an instruction, such as code size and
-- latency.

data InstProperties
    = InstProperties {

          -- | Instruction code size (in bytes).

          instCodeSize :: Integer

          -- | Instruction latency (in cycles).

        , instLatency :: Integer

      }
    deriving (Show)

-- | Defines a pattern for a machine instruction.

data InstPattern
    = InstPattern {

          -- | The ID of this pattern. The ID must be unique within the same
          -- instruction, but not necessarily contiguous.

          patID :: PatternID

          -- | The operation structure of the pattern.

        , patOS :: OpStructure

          -- | Specifies the data nodes within the 'OpStructure' which represent
          -- output that can be observed from outside the pattern.

        , patOutputDataNodes :: [NodeID]

          -- | Indicates whether the use-def-dom constraints apply to this
          -- pattern. This will typically always be set to 'True' for all
          -- patterns except the generic phi patterns.

        , patAUDDC :: Bool

          -- | Maps an 'AssemblyID', which is denoted as the index into the
          -- list, that appear in the 'AssemblyString' of the instruction, to a
          -- particular node in the graph of the pattern's operation structure.
          -- Because of this, all 'AssemblyID's used within the same
          -- 'AssemblyString' *must* be unique and contiguous!

        , patAssIDMaps :: [NodeID]

      }
    deriving (Show)
