--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.OpTypes.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains data types for operations.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.OpTypes.Base where



--------------------------------------------------
-- Data types
--------------------------------------------------

-- | Unary operation types.

data UnaryOp

      -- | Unsigned arithmetic square root function.

    = USqrt

      -- | Floating-point square root function.

    | Sqrt

      -- | Signed arithmetic square root function.

    | FixPointSqrt

      -- | Bit-wise NOT (@|@).

    | Not

    deriving (Show, Eq)

-- | Binary operation types.

data BinaryOp
    = BinArithmeticOp ArithmeticOp
    | BinCompareOp CompareOp
    deriving (Show, Eq)

-- | Arithmetic operation types.

data ArithmeticOp

    ------------------------------
    -- Meta operations
    ------------------------------

      -- | Addition. Commutative.

    = Plus

      -- | Subtraction. Commutative.

    | Minus

    ------------------------------
    -- Integer operations
    ------------------------------

      -- | Integer addition. Commutative.

    | IAdd

      -- | Integer saturated addition. Commutative.

    | ISatAdd

      -- | Integer subtraction.

    | ISub

      -- | Integer saturated subtraction. Commutative.

    | ISatSub

      -- | Integer multiplication. Commutative.

    | IMul

      -- | Integer saturated multiplication. Commutative.

    | ISatMul

      -- | Unsigned integer division.

    | IUDiv

      -- | Signed integer division.

    | ISDiv

      -- | Unsigned integer remainder.

    | IURem

      -- | Signed integer remainder.

    | ISRem

    ------------------------------
    -- Fix-point operations
    ------------------------------

      -- | Fix-point division.

    | FixPointDiv

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

      -- | Zero-extension. LHS denotes the value to zero extend, and RHS denotes
      -- the amount of bits to extend with.

    | ZExt

      -- | Sign-extension. LHS denotes the value to sign extend, and RHS denotes
      -- the amount of bits to extend with.

    | SExt

    deriving (Show, Eq)

-- | Comparison operation types.

data CompareOp

    ------------------------------
    -- Integer operations
    ------------------------------

      -- | Integer equality comparison (@==@). Commutative.

    = ICmpEq

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
