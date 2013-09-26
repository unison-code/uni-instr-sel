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

module Language.InstructionSelection.Patterns.Base where

import Language.InstructionSelection.Patterns.AssemblyString
import Data.Graph.Inductive



-- | TODO: write description

data NodeType
      -- | TODO: implement
    = NodeType
    deriving (Show)

-- | TODO: write description

data EdgeType
      -- | TODO: implement
    = EdgeType
    deriving (Show)

-- | TODO: write description

data Constraint
      -- | TODO: implement
    = Constraint
    deriving (Show)

-- | Data type for representing a pattern including the constraints.

data Pattern
    = Pattern

          -- | The pattern graph.

          (Gr (LNode NodeType) (LEdge EdgeType))

          -- | Constraints that must be enforced for the pattern.

          [Constraint]

    deriving (Show)

-- | Data type for representing an instruction.

data Instruction
    = Instruction

          -- | Assembly string to produce upon code emission.

          AssemblyString

          -- | Patterns which correspond to the instruction. There must be at
          -- least one pattern.

          [Pattern]

    deriving (Show)