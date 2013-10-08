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
import Language.InstructionSelection.Utils
import Language.InstructionSelection.OpTypes



data Label
    = Label String
    deriving (Show)

data NodeType
    = NTBinaryOp BinaryOp
    | NTUnaryOp UnaryOp
    | NTMemoryLoad
    | NTMemoryStore
    | NTUncondBranch Label
    | NTCondBranch

          -- | Label taken if the register evaluates to @True@.

          Label

          -- | Label taken if the register evaluates to @False@.

          Label

    | NTPhi
    | NTData
    deriving (Show)

data NodeLabel
    = NodeLabel

      -- | Node identifier.

      Natural

      NodeType

    deriving (Show)

data EdgeLabel
    = EdgeType

      -- | Source identifier.

      Natural

      -- | Destination identifier.

      Natural

    deriving (Show)

data Constraint
      -- | TODO: implement
    = Constraint
    deriving (Show)

data Pattern
    = Pattern

          -- | The pattern graph.

          (Gr (LNode NodeLabel) (LEdge EdgeLabel))

          -- | Constraints that must be enforced for the pattern.

          [Constraint]

    deriving (Show)

data Instruction
    = Instruction

          -- | Assembly string to produce upon code emission.

          AssemblyString

          -- | Patterns which correspond to the instruction. There must be at
          -- least one pattern.

          [Pattern]

    deriving (Show)
