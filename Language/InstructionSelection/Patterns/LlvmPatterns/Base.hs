--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.LlvmPatterns.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data types and records for representing instructions as LLVM
-- patterns.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Patterns.LlvmPatterns.Base where

import Language.InstructionSelection.Misc (Range)
import Language.InstructionSelection.Misc (BinaryOp)
import Language.InstructionSelection.Misc (ArithmeticOp)
import Language.InstructionSelection.Misc (CompareOp)



-- | Record for a temporary.

data Temporary
    = Temporary

          -- | Identifier for the temporary. All temporaries are required to
          -- have a unique identifier within the function scope.

          Integer

    deriving (Show, Eq)

-- | Record for describing a register symbol.

data RegisterSymbol
    = RegisterSymbol String
    deriving (Show)

-- | Record for describing a register flag symbol.

data RegisterFlagSymbol
    = RegisterFlagSymbol String
    deriving (Show)

-- | Record for describing a data space.

data DataSpace

      -- | Constructor for a register class.

    = RegisterClass

          -- | Identifier for the class.

          String

    | MemoryClass

          -- | Identifier for the class.

          String

          -- | Address range within the memory class. This may be the entire
          -- range of the memory space, or a restricted subset.

          (Range Integer)

    deriving (Show, Eq)

-- | Record for an immediate value symbol. This is used to represent an
-- immediate value which will become available during pattern matching.

data ImmediateSymbol
    = ImmediateSymbol String
    deriving (Show, Eq)

-- | Record for describing data. The data can be of many different types, e.g. a
-- constant, an immediate, a temporary, etc.

data Data

      -- | A value which is fixed and known at compile time.

    = ConstantData Integer

      -- | An immediate value represented by a symbol.

    | ImmediateData ImmediateSymbol

      -- | A value located in a temporary.

    | TemporaryData Temporary

    deriving (Show, Eq)

-- | Record for containing an potentially nested expression.

data Expression

      -- | A binary expression. The first expression is the LHS and the second
      -- expression is the RHS.

    = BinaryOpExpr
          BinaryOp

          -- | LHS.

          Expression

          -- | RHS.

          Expression

      -- | A phi expression.

    | PhiExpr [PhiExpression]

      -- | A data expression.

    | DataExpr Data

    deriving (Show)

-- | Record for containing an expression inside a phi function. The label
-- indicates from where the value comes from.

data PhiExpression
    = PhiExpression Expression Label
    deriving (Show)

-- | Record for containing a label.

data Label
    = Label

          -- | Label identifier.

          String

    deriving (Show, Eq)

-- | Record for representing an LLVM statement.

data Statement

      -- | Assigns the result of an expression to a temporary.

    = AssignmentStmt Temporary Expression

      -- | Performs an unconditional branch (or jump) to a label.

    | BranchStmt Label

      -- | Declares a label.

    | LabelStmt Label

    deriving (Show)

-- | Record for a pattern constraint.

data Constraint

      -- | The @AllocateIn@ constraint dictates that a register value must be
      -- located in a particular space.

    = AllocateIn RegisterSymbol DataSpace

      -- | The @ImmediateRange@ constraint limits the range of values that an
      -- immediate value may take.

    | ImmediateRange ImmediateSymbol (Range Integer)

      -- | The @Alias@ constraint dictates that two temporaries must be the
      -- same, in the sense that both temporaries must be assigned the same
      -- register.

    | Alias Temporary Temporary

      -- | TODO: add description

    | Assert AssertExpression

    deriving (Show)

-- | Record for containing an assert expression.

data AssertExpression

      -- | Checks whether a register is within a certain data space.

    = ContainsExpr RegisterSymbol DataSpace

      -- | Checks whether a comparison between an immediate symbol and a
      -- constant integer value holds.

    | CompareExpr CompareOp ImmediateSymbol Integer

      -- | Checks whether a certain flag in a register is set.

    | RegFlagExpr RegisterFlagSymbol RegisterSymbol

      -- | Negates an assert expression.

    | NotExpr AssertExpression

    deriving (Show)

-- | Record for containing the assembly string to produce during code emission.

data AssemblyString
    = AssemblyString
          -- | TODO: refactor into something that is easier to process.
          String
    deriving (Show)

-- | Record for representing a pattern including the constraints.

data Pattern
    = Pattern

          -- | The LLVM statements.

          [Statement]

          -- | Constraints that must be enforced for the pattern.

          [Constraint]

    deriving (Show)

-- | Record for representing an instruction.

data Instruction
    = Instruction

          -- | Assembly string to produce upon code emission.

          AssemblyString

          -- | Patterns which correspond to the instruction. There must be at
          -- least one pattern.

          [Pattern]

    deriving (Show)
