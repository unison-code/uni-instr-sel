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

import Language.InstructionSelection.Graphs
import Language.InstructionSelection.Patterns.AssemblyString
import Language.InstructionSelection.Utils (Range (..))



data Register
    = Register String
    deriving (Show,Eq)

data Constant
    = IntConstant Integer
    deriving (Show,Eq)

data Constraint
    = Constraint
    | AllocateInRegisterConstraint NodeId [Register]
    | ConstantValueConstraint NodeId [Range Constant]
    | IsAliasConstraint NodeId NodeId
    deriving (Show,Eq)

data Pattern
    = Pattern
          Graph
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
