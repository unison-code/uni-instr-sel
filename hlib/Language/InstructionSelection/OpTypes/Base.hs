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

-- | Computational operations.

data CompOp

    -- | An unsigned integer operation.

    = UIntOp CompOpType

    -- | A signed integer operation.

    | SIntOp CompOpType

    -- | A fixed-point operation.

    | FixpointOp CompOpType

    -- | A floating-point operation.

    | FloatOp CompOpType

    deriving (Show, Eq)

-- | Computational operation types.

data CompOpType

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

data ControlOp

    -- | Conditional branch. Branching is done if the input value is not zero.

    = CondBranch

    -- | Unconditional branch.

    | UncondBranch

    -- | Return.

    | Ret

    deriving (Show, Eq)



--------------
-- Functions
--------------

-- | Gets the operation type from a computational operation.

getCompOpType :: CompOp -> CompOpType
getCompOpType (UIntOp o) = o
getCompOpType (SIntOp o) = o
getCompOpType (FixpointOp o) = o
getCompOpType (FloatOp o) = o

-- | Checks if an operation is commutative.

isOpCommutative :: CompOp -> Bool
isOpCommutative op = isOpTypeCommutative $ getCompOpType op

-- | Checks if an operation type is commutative.

isOpTypeCommutative :: CompOpType -> Bool
isOpTypeCommutative op =
  op `notElem` [ Sub, SatSub, Div, Rem, Shl, LShr, AShr, GT, GE, LT, LE ]



------------------------
-- Type class instances
------------------------

instance PrettyPrint CompOp where
  prettyShow = prettyShow . getCompOpType

instance PrettyPrint CompOpType where
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

instance PrettyPrint ControlOp where
  prettyShow CondBranch = "bnz"
  prettyShow UncondBranch = "br"
  prettyShow Ret = "ret"

instance SExpressionable CompOp where
  prettySE (UIntOp o) i     = "u" ++ (prettySE o i)
  prettySE (SIntOp o) i     = "s" ++ (prettySE o i)
  prettySE (FixpointOp o) i = "fixp" ++ (prettySE o i)
  prettySE (FloatOp o) i    = "f" ++ (prettySE o i)

instance SExpressionable CompOpType where
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

instance SExpressionable ControlOp where
  prettySE CondBranch _ = "bnz"
  prettySE UncondBranch _ = "br"
  prettySE Ret _ = "ret"
