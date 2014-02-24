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

import Language.InstructionSelection.PrettyPrint
import Language.InstructionSelection.SExpressions
import Prelude hiding (GT, LT)



--------------
-- Data types
--------------

-- | Operations.

data Operation

    -- | An unsigned integer operation.

    = UIntOp OpType

    -- | A signed integer operation.

    | SIntOp OpType

    -- | A fixed-point operation.

    | FixpointOp OpType

    -- | A floating-point operation.

    | FloatOp OpType

    deriving (Show, Eq)


-- | Operation types.

data OpType

      -- | Addition. Commutative.

    = Add

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

      -- | Bit-wise NOT (@|@).

    | Not

      -- | Equality comparison (@==@). Commutative.

    | Eq

      -- | Inequality comparison (@!=@). Commutative.

    | NEq

      -- | Greater-than comparison (@>@). If LHS is denoted by @x@, and RHS is
      -- denoted by @y@, then this operation represents @x > y@.

    | GT

      -- | Greater-than-or-equal comparison (@>=@). If LHS is denoted by @x@,
      -- and RHS is denoted by @y@, then this operation represents @x >= y@.

    | GE

      -- | Less-than comparison (@<@). If LHS is denoted by @x@, and RHS is
      -- denoted by @y@, then this operation represents @x < y@.

    | LT

      -- | Less-than-or-equal comparison (@<=@). If LHS is denoted by @x@, and
      -- RHS is denoted by @y@, then this operation represents @x <= y@.

    | LE

      -- | Zero extension.

    | ZExt

      -- | Sign extension.

    | SExt

      -- | Truncation.

    | Trunc

      -- | Square root function.

    | Sqrt

    deriving (Show, Eq)



--------------
-- Functions
--------------

-- | Gets the operation type from an operation.

getOpType :: Operation -> OpType
getOpType (UIntOp o) = o
getOpType (SIntOp o) = o
getOpType (FixpointOp o) = o
getOpType (FloatOp o) = o



-------------------------
-- Class implementations
-------------------------

instance PrettyPrint Operation where
  prettyShow = prettyShow . getOpType

instance PrettyPrint OpType where
  prettyShow Add    = "+"
  prettyShow SatAdd = "+"
  prettyShow Sub    = "-"
  prettyShow SatSub = "-"
  prettyShow Mul    = "*"
  prettyShow Div    = "/"
  prettyShow Rem    = "%"
  prettyShow Shl    = "<<"
  prettyShow LShr   = ">>"
  prettyShow AShr   = ">>"
  prettyShow And    = "&&"
  prettyShow Or     = "||"
  prettyShow Xor    = "^"
  prettyShow Not    = "!"
  prettyShow Eq     = "=="
  prettyShow NEq    = "!="
  prettyShow GT     = ">"
  prettyShow GE     = ">="
  prettyShow LT     = "<"
  prettyShow LE     = "<="
  prettyShow ZExt   = "zext"
  prettyShow SExt   = "sext"
  prettyShow Trunc  = "trunc"
  prettyShow Sqrt   = "sqrt"

instance SExpressionable Operation where
  prettySE (UIntOp o) i     = "u" ++ (prettySE o i)
  prettySE (SIntOp o) i     = "s" ++ (prettySE o i)
  prettySE (FixpointOp o) i = "fixp" ++ (prettySE o i)
  prettySE (FloatOp o) i    = "f" ++ (prettySE o i)

instance SExpressionable OpType where
  prettySE Add    _ = "add"
  prettySE SatAdd _ = "satadd"
  prettySE Sub    _ = "sub"
  prettySE SatSub _ = "satsub"
  prettySE Mul    _ = "mul"
  prettySE Div    _ = "div"
  prettySE Rem    _ = "rem"
  prettySE Shl    _ = "shl"
  prettySE LShr   _ = "lshr"
  prettySE AShr   _ = "ashr"
  prettySE And    _ = "and"
  prettySE Or     _ = "or"
  prettySE Xor    _ = "xor"
  prettySE Not    _ = "not"
  prettySE Eq     _ = "eq"
  prettySE NEq    _ = "neq"
  prettySE GT     _ = "gt"
  prettySE GE     _ = "ge"
  prettySE LT     _ = "lt"
  prettySE LE     _ = "le"
  prettySE ZExt   _ = "zext"
  prettySE SExt   _ = "sext"
  prettySE Trunc  _ = "trunc"
  prettySE Sqrt   _ = "sqrt"
