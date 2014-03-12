--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Patterns.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data types and records for representing patterns. This is the
-- format on which subsequent preparation for instruction selection will build
-- on (i.e. other pattern forms, such as those based on LLVM, will be converted
-- into this format).
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Patterns.Base (
  Instruction (..)
, InstProperties (..)
, PatternId
) where

import Language.InstructionSelection.OpStructures
import Language.InstructionSelection.Patterns.AssemblyString
import Language.InstructionSelection.Utils (Natural)



type PatternId = Natural

data Instruction
    = Instruction {

          -- | Assembly string to produce upon code emission.

          assemblyStr :: AssemblyString

          -- | Instruction properties.

        , instProps :: InstProperties

          -- | Patterns which correspond to the instruction. There must be at
          -- least one pattern. Each pattern also has a corresponding ID which
          -- must be globally unique across all instructions, but not
          -- necessarily contiguous.

        , patterns :: [(OpStructure, PatternId)]

      }
    deriving (Show)

data InstProperties
    = InstProperties {

          -- | Instruction code size (in bytes).

          codeSize :: Integer

          -- | Instruction latency (in cycles).

        , latency :: Integer

      }
    deriving (Show)
