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
    CompArithOp ArithOp
  | CompTypeConvOp TypeConvOp
  deriving (Show, Eq)

-- | Arithmetic operations.
data ArithOp =
    -- | An integer operation where the sign does not matter.
    IntOp ArithOpType

    -- | An unsigned integer operation.
  | UIntOp ArithOpType

    -- | A signed integer operation.
  | SIntOp ArithOpType

    -- | A fixed-point operation.
  | FixpointOp ArithOpType

    -- | A floating-point operation where the ordering does not matter.
  | FloatOp ArithOpType

    -- | An ordered floating-point operation.
  | OFloatOp ArithOpType

    -- | An unordered floating-point operation.
  | UFloatOp ArithOpType
  deriving (Show, Eq)

-- | Arithmetic operation types.
data ArithOpType =
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
  | CondBranch

    -- | Return.
  | Ret
  deriving (Show, Eq)



--------------
-- Functions
--------------

-- | Gets the operation type from an arithmetic operation.
getArithOpType :: ArithOp -> ArithOpType
getArithOpType ( IntOp op)     = op
getArithOpType (UIntOp op)     = op
getArithOpType (SIntOp op)     = op
getArithOpType (FixpointOp op) = op
getArithOpType ( FloatOp op)   = op
getArithOpType (OFloatOp op)   = op
getArithOpType (UFloatOp op)   = op

-- | Checks if an operation is commutative. Unary operations are always
-- considered to be commutative.
isOpCommutative :: CompOp -> Bool
isOpCommutative (CompArithOp op) = isArithOpCommutative op
isOpCommutative (CompTypeConvOp _) = True

-- | Checks if an arithmetic operation is commutative.
isArithOpCommutative :: ArithOp -> Bool
isArithOpCommutative = isArithOpTypeCommutative . getArithOpType

-- | Checks if an arithmetic operation type is commutative. Unary operations are
-- always considered to be commutative.
isArithOpTypeCommutative :: ArithOpType -> Bool
isArithOpTypeCommutative op =
  op `notElem` [ Sub, SatSub, Div, Rem, Shl, LShr, AShr, GT, GE, LT, LE ]

-- | Gets the number of operands required by a given operation.
numOperandsForCompOp :: CompOp -> Natural
numOperandsForCompOp (CompArithOp op) = numOperandsForArithOp op
numOperandsForCompOp (CompTypeConvOp _) = 1

-- | Gets the number of operands required by a given arithmetic operation.
numOperandsForArithOp :: ArithOp -> Natural
numOperandsForArithOp = numOperandsForArithOpType . getArithOpType

-- | Gets the number of operands required by a given arithmetic operation type.
numOperandsForArithOpType :: ArithOpType -> Natural
numOperandsForArithOpType op
  | op `elem` [ Not, Sqrt ] = 1
  | otherwise = 2

-- | Checks if two computations are compatible, meaning that they are
-- semantically equivalent.
areCompOpsCompatible :: CompOp -> CompOp -> Bool
areCompOpsCompatible (CompArithOp op1) (CompArithOp op2) =
  areArithOpsCompatible op1 op2
areCompOpsCompatible (CompTypeConvOp op1) (CompTypeConvOp op2) =
  areTypeConvOpsCompatible op1 op2
areCompOpsCompatible _ _ = False

-- | Checks if two arithmetic operations are compatible, meaning that they are
-- semantically equivalent.
areArithOpsCompatible :: ArithOp -> ArithOp -> Bool
areArithOpsCompatible  ( IntOp op1)    ( IntOp op2) = op1 == op2
areArithOpsCompatible  ( IntOp op1)    (UIntOp op2) = op1 == op2
areArithOpsCompatible  ( IntOp op1)    (SIntOp op2) = op1 == op2
areArithOpsCompatible  (UIntOp op1)    ( IntOp op2) = op1 == op2
areArithOpsCompatible  (SIntOp op1)    ( IntOp op2) = op1 == op2
areArithOpsCompatible ( FloatOp op1) ( FloatOp op2) = op1 == op2
areArithOpsCompatible ( FloatOp op1) (OFloatOp op2) = op1 == op2
areArithOpsCompatible ( FloatOp op1) (UFloatOp op2) = op1 == op2
areArithOpsCompatible (UFloatOp op1) ( FloatOp op2) = op1 == op2
areArithOpsCompatible (OFloatOp op1) ( FloatOp op2) = op1 == op2
areArithOpsCompatible op1 op2                       = op1 == op2

-- | Checks if two type conversion operations are compatible, meaning that they
-- are semantically equivalent.
areTypeConvOpsCompatible :: TypeConvOp -> TypeConvOp -> Bool
areTypeConvOpsCompatible = (==)

------------------------
-- Type class instances
------------------------

instance PrettyPrint CompOp where
  prettyShow (CompArithOp op) = prettyShow $ getArithOpType op
  prettyShow (CompTypeConvOp op) = prettyShow op

instance PrettyPrint ArithOpType where
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
  prettyShow Branch     = "br"
  prettyShow CondBranch = "cbr"
  prettyShow Ret        = "ret"
