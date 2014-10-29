--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.OpTypes.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains data types for operations.
--
--------------------------------------------------------------------------------

module Language.InstSel.OpTypes.Base where

import Language.InstSel.PrettyPrint
import Language.InstSel.Utils
  ( Natural )
import Prelude
  hiding
  ( GT
  , LT
  )



--------------
-- Data types
--------------

-- | Computational operations.
data CompOp =
    -- | An integer operation where the sign does not matter.
    IntOp CompOpType

    -- | An unsigned integer operation.
  | UIntOp CompOpType

    -- | A signed integer operation.
  | SIntOp CompOpType

    -- | A fixed-point operation.
  | FixpointOp CompOpType

    -- | A floating-point operation where the ordering does not matter.
  | FloatOp CompOpType

    -- | An ordered floating-point operation.
  | OFloatOp CompOpType

    -- | An unordered floating-point operation.
  | UFloatOp CompOpType
  deriving (Show, Eq)

-- | Computational operation types.
data CompOpType =
    -- | Addition. Commutative.
    Add

    -- | Saturated addition. Commutative. Typically only used with fixpoint.
  | SatAdd

    -- | Subtraction.
  | Sub

    -- | Saturated subtraction. Typically only used with fixpoint.
  | SatSub

    -- | Multiplication. Commutative.
  | Mul

    -- | Division.
  | Div

    -- | Remainder.
  | Rem

    -- | Bitwise left shift. If LHS is denoted by @x@, and RHS is denoted by
    -- @y@, then this operation represents @x < y@.
  | Shl

    -- | Bitwise logical right shift. If LHS is denoted by @x@, and RHS is
    -- denoted by @y@, then this operation represents @x > y@.
  | LShr

    -- | Bitwise arithmetic right shift (with sign extension). If LHS is denoted
    -- by @x@, and RHS is denoted by @y@, then this operation represents @x >
    -- y@.
  | AShr

    -- | Bitwise AND (@\&@). Commutative.
  | And

    -- | Bitwise OR (@|@). Commutative.
  | Or

    -- | Bitwise XOR (@^@). Commutative.
  | XOr

    -- | Bit-wise NOT (@|@).
  | Not

    -- | Equality comparison (@==@). Commutative.
  | Eq

    -- | Inequality comparison (@!=@). Commutative.
  | NEq

    -- | Greater-than comparison (@>@). If LHS is denoted by @x@, and RHS is
    -- denoted by @y@, then this operation represents @x > y@.
  | GT

    -- | Greater-than-or-equal comparison (@>=@). If LHS is denoted by @x@, and
    -- RHS is denoted by @y@, then this operation represents @x >= y@.
  | GE

    -- | Less-than comparison (@<@). If LHS is denoted by @x@, and RHS is
    -- denoted by @y@, then this operation represents @x < y@.
  | LT

    -- | Less-than-or-equal comparison (@<=@). If LHS is denoted by @x@, and RHS
    -- is denoted by @y@, then this operation represents @x <= y@.
  | LE

    -- | Square root function.
  | Sqrt

    -- | Checks if both floating point values are ordered.
  | Ordered

    -- | Checks if either floating point value is unordered.
  | Unordered
  deriving (Show, Eq)

-- | Operations that convert values of one type to another type.
data TypeConvOp =
    -- | Zero extension.
    ZExt

    -- | Sign extension.
  | SExt

    -- | Truncation.
  | Trunc

    -- | Floating-point to signed integer.
  | Float2SInt

    -- | Floating-point to unsigned integer.
  | Float2UInt

    -- | Signed integer to floating-point.
  | SInt2Float

    -- | Unsigned integer to floating-point.
  | UInt2Float

  deriving (Show, Eq)

data ControlOp =
    -- | Unconditional branch (same as a jump).
    Branch

    -- | Conditional branch. Branching is done if the input value is not zero.
  | IfBranch

    -- | Return.
  | Ret
  deriving (Show, Eq)



--------------
-- Functions
--------------

-- | Gets the operation type from a computational operation.
getCompOpType :: CompOp -> CompOpType
getCompOpType ( IntOp op)     = op
getCompOpType (UIntOp op)     = op
getCompOpType (SIntOp op)     = op
getCompOpType (FixpointOp op) = op
getCompOpType ( FloatOp op)   = op
getCompOpType (OFloatOp op)   = op
getCompOpType (UFloatOp op)   = op

-- | Checks if an operation is commutative.
isOpCommutative :: CompOp -> Bool
isOpCommutative = isOpTypeCommutative . getCompOpType

-- | Checks if an operation type is commutative. Unary operations are always
-- considered to be commutative.
isOpTypeCommutative :: CompOpType -> Bool
isOpTypeCommutative op =
  op `notElem` [ Sub, SatSub, Div, Rem, Shl, LShr, AShr, GT, GE, LT, LE ]

-- | Gets the number of operands required by a given operation.
numOperandsForOp :: CompOp -> Natural
numOperandsForOp = numOperandsForOpType . getCompOpType

-- | Gets the number of operands required by a given operation type.
numOperandsForOpType :: CompOpType -> Natural
numOperandsForOpType op
  | op `elem` [ Not, Sqrt ] = 1
  | otherwise = 2

-- | Checks if two computations are compatible, meaning that they are
-- semantically equivalent.
areComputationsCompatible :: CompOp -> CompOp -> Bool
areComputationsCompatible  ( IntOp op1)    ( IntOp op2) = op1 == op2
areComputationsCompatible  ( IntOp op1)    (UIntOp op2) = op1 == op2
areComputationsCompatible  ( IntOp op1)    (SIntOp op2) = op1 == op2
areComputationsCompatible  (UIntOp op1)    ( IntOp op2) = op1 == op2
areComputationsCompatible  (SIntOp op1)    ( IntOp op2) = op1 == op2
areComputationsCompatible ( FloatOp op1) ( FloatOp op2) = op1 == op2
areComputationsCompatible ( FloatOp op1) (OFloatOp op2) = op1 == op2
areComputationsCompatible ( FloatOp op1) (UFloatOp op2) = op1 == op2
areComputationsCompatible (UFloatOp op1) ( FloatOp op2) = op1 == op2
areComputationsCompatible (OFloatOp op1) ( FloatOp op2) = op1 == op2
areComputationsCompatible op1 op2                       = op1 == op2



------------------------
-- Type class instances
------------------------

instance PrettyPrint CompOp where
  prettyShow = prettyShow . getCompOpType

instance PrettyPrint CompOpType where
  prettyShow Add       = "+"
  prettyShow SatAdd    = "+"
  prettyShow Sub       = "-"
  prettyShow SatSub    = "-"
  prettyShow Mul       = "*"
  prettyShow Div       = "/"
  prettyShow Rem       = "%"
  prettyShow Shl       = "<<"
  prettyShow LShr      = ">>"
  prettyShow AShr      = ">>"
  prettyShow And       = "&&"
  prettyShow Or        = "||"
  prettyShow XOr       = "^"
  prettyShow Not       = "!"
  prettyShow Eq        = "=="
  prettyShow NEq       = "!="
  prettyShow GT        = ">"
  prettyShow GE        = ">="
  prettyShow LT        = "<"
  prettyShow LE        = "<="
  prettyShow Sqrt      = "sqrt"
  prettyShow Ordered   = "ord"
  prettyShow Unordered = "uno"

instance PrettyPrint TypeConvOp where
  prettyShow ZExt  = "zext"
  prettyShow SExt  = "sext"
  prettyShow Trunc = "trunc"
  prettyShow Float2SInt = "fptosi"
  prettyShow Float2UInt = "fptoui"
  prettyShow SInt2Float = "sitofp"
  prettyShow UInt2Float = "uitofp"

instance PrettyPrint ControlOp where
  prettyShow Branch   = "br"
  prettyShow IfBranch = "bnz"
  prettyShow Ret      = "ret"
