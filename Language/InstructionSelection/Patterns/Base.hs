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
) where

import Language.InstructionSelection.OperationStructures
import Language.InstructionSelection.Patterns.AssemblyString



data Instruction
    = Instruction {

          -- | Assembly string to produce upon code emission.

          assemblyStr :: AssemblyString

          -- | Patterns which correspond to the instruction. There must be at
          -- least one pattern.

        , patterns :: [OpStructure]

      }

    deriving (Show)
