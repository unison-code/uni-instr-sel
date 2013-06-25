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

-- | Binary operation types.

data BinaryOp

    ------------------------------------------------------------
    -- Arithmetic and logical operations
    ------------------------------------------------------------

    ------------------------------
    -- Integer operations
    ------------------------------

      -- | Integer addition. Commutative.

    = IAdd

      -- | Integer subtraction.

    | ISub

      -- | Integer multiplication. Commutative.

    | IMul

      -- | Unsigned integer division.

    | IUDiv

      -- | Signed integer division.

    | ISDiv

      -- | Unsigned integer remainder.

    | IURem

      -- | Signed integer remainder.

    | ISRem

    ------------------------------
    -- Floating-point operations
    ------------------------------

      -- | Float addition. Commutative.

    | FAdd

      -- | Float subtraction.

    | FSub

      -- | Float multiplication. Commutative.

    | FMul

      -- | Float division.

    | FDiv

      -- | Float remainder.

    | FRem

    ------------------------------
    -- Bit operations
    ------------------------------

      -- | Bitwise left shift. If LHS is denoted by @x@, and RHS is denoted
      -- by @y@, then this operation represents @x < y@.

    | Shl

      -- | Bitwise logical right shift. If LHS is denoted by @x@, and RHS is
      -- denoted by @y@, then this operation represents @x > y@.

    | LShr

      -- | Bitwise arithmetic right shift (with sign extension). If LHS is
      -- denoted by @x@, and RHS is denoted by @y@, then this operation
      -- represents @x > y@.

    | AShr

      -- | Bitwise AND (@\&@). Commutative.

    | And

      -- | Bitwise OR (@|@). Commutative.

    | Or

      -- | Bitwise XOR (@^@). Commutative.

    | Xor

    ------------------------------------------------------------
    -- Comparison operations
    ------------------------------------------------------------

    ------------------------------
    -- Integer operations
    ------------------------------

      -- | Integer equality comparison (@==@). Commutative.

    | ICmpEq

      -- | Integer inequality comparison (@!=@). Commutative.

    | ICmpNEq

      -- | Unsigned integer greater-than comparison (@>@). If LHS is denoted
      -- by @x@, and RHS is denoted by @y@, then this operation represents
      -- @x > y@.

    | IUCmpGT

      -- | Signed integer greater-than comparison (@>@). Same as for 'IUCmpGT'
      -- but for signed integer data.

    | ISCmpGT

      -- | Unsigned integer greater-than-or-equal comparison (@>=@). If LHS is
      -- denoted by @x@, and RHS is denoted by @y@, then this operation
      -- represents @x >= y@.

    | IUCmpGE

      -- | Signed integer greater-than-or-equal comparison (@>=@). Same as for
      -- 'IUCmpGE' but for signed integer data.

    | ISCmpGE

      -- | Unsigned integer less-than comparison (@<@). If LHS is denoted by
      -- @x@, and RHS is denoted by @y@, then this operation represents @x < y@.

    | IUCmpLT

      -- | Signed integer less-than comparison (@<@). Same as for 'IUCmpLT' but
      -- for signed integer data.

    | ISCmpLT

      -- | Unsigned integer less-than-or-equal comparison (@<=@). If LHS is
      -- denoted by @x@, and RHS is denoted by @y@, then this operation
      -- represents @x <= y@.

    | IUCmpLE

      -- | Signed integer less-than-or-equal comparison (@<=@). Same as for
      -- 'IUCmpLE' but for signed integer data.

    | ISCmpLE

    ------------------------------
    -- Floating-point operations
    ------------------------------

      -- | Unordered float equality comparison (@==@). If any of the values is a
      -- QNaN or both values are equal, then the operation returns
      -- @True@. Commutative.

    | FUCmpEq

      -- | Ordered float inequality comparison (@!=@). If none of the values is
      -- a QNaN and both values are equal, then the operation returns
      -- @True@. Commutative.

    | FOCmpEq

      -- | Unordered float inequality comparison (@!=@). If any of the values is
      -- a QNaN or both values are inequal, then the operation returns
      -- @True@. Commutative.

    | FUCmpNEq

      -- | Ordered float inequality comparison (@!=@). If none of the values is
      -- a QNaN and both values are inequal, then the operation returns
      -- @True@. Commutative.

    | FOCmpNEq

      -- | Unordered float greater-than comparison (@>@). If LHS is denoted by
      -- @x@, and RHS is denoted by @y@, then this operation represents @x > y@.
      -- Hence, if any of the values is a QNaN or @x > y@ holds, then the
      -- operation returns @True@.

    | FUCmpGT

      -- | Ordered float greater-than comparison (@>@). If LHS is denoted by
      -- @x@, and RHS is denoted by @y@, then this operation represents @x > y@.
      -- Hence, if none of the values is a QNaN and @x > y@ holds, then the
      -- operation returns @True@.

    | FOCmpGT

      -- | Unordered float greater-than-or-equal comparison (@>=@). If LHS is
      -- denoted by @x@, and RHS is denoted by @y@, then this operation
      -- represents @x >= y@. Hence, if any of the values is a QNaN or @x >= y@
      -- holds, then the operation returns @True@.

    | FUCmpGE

      -- | Ordered float greater-than-or-equal comparison (@>=@). If LHS is
      -- denoted by @x@, and RHS is denoted by @y@, then this operation
      -- represents @x >= y@. Hence, if none of the values is a QNaN and
      -- @x >= y@ holds, then the operation returns @True@.

    | FOCmpGE

      -- | Unordered float less-than comparison (@<@). If LHS is denoted by @x@,
      -- and RHS is denoted by @y@, then this operation represents @x < y@.
      -- Hence, if any of the values is a QNaN or @x < y@ holds, then the
      -- operation returns @True@.

    | FUCmpLT

      -- | Ordered float less-than comparison (@<@). If LHS is denoted by @x@,
      -- and RHS is denoted by @y@, then this operation represents @x < y@.
      -- Hence, if none of the values is a QNaN and @x < y@ holds, then the
      -- operation returns @True@.

    | FOCmpLT

      -- | Unordered float less-than-or-equal comparison (@<=@). If LHS is
      -- denoted by @x@, and RHS is denoted by @y@, then this operation
      -- represents @x <= y@. Hence, if any of the values is a QNaN or @x <= y@
      -- holds, then the operation returns @True@.

    | FUCmpLE

      -- | Ordered float less-than-or-equal comparison (@<=@). If LHS is denoted
      -- by @x@, and RHS is denoted by @y@, then this operation represents
      -- @x <= y@. Hence, if none of the values is a QNaN and @x <= y@ holds,
      -- then the operation returns @True@.

    | FOCmpLE

      -- | Float unordering check. If any of the values is a QNaN, then the
      -- operation returns @True@. Commutative.

    | FCmpUn

    deriving (Show, Eq)

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

      -- | An integer value which is fixed and known at compile time.

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

    | Assert {
          -- | TODO: refactor into more exact data types
          condition :: String
      }

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
