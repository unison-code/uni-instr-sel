--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.OpTypes.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains data types for operations.
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstrSel.OpTypes.Base where

import Language.InstrSel.DebugShow
import Language.InstrSel.Utils
  ( Natural
  , splitOn
  )
import Language.InstrSel.Utils.JSON
import Data.Maybe
  ( fromJust
  , isJust
  )

import Prelude
  hiding
  ( GT
  , LT
  )



--------------
-- Data types
--------------

-- | Computational operations.
data CompOp
  = CompArithOp ArithOp
  | CompTypeConvOp TypeConvOp
  deriving (Eq)

-- | Arithmetic operations.
data ArithOp
    -- | An integer operation where the sign does not matter.
  = IntOp ArithOpType
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
  deriving (Eq)

-- | Arithmetic operation types.
data ArithOpType
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
  deriving (Eq)

-- | Operations that convert values of one type to another type.
data TypeConvOp
    -- | Zero extension.
  = ZExt
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
  deriving (Eq)

data ControlOp
    -- | Unconditional branch (same as a jump).
  = Br
    -- | Conditional branch. Branching is done if the input value is not zero.
  | CondBr
    -- | Return.
  | Ret
  deriving (Eq)



-------------------------------------
-- Show-related type class instances
-------------------------------------

instance Show CompOp where
  show (CompArithOp op) = show op
  show (CompTypeConvOp op) = show op

instance Show ArithOp where
  show (IntOp op)      = "i "  ++ show op
  show (UIntOp op)     = "ui " ++ show op
  show (SIntOp op)     = "si " ++ show op
  show (FixpointOp op) = "fix "++ show op
  show (FloatOp op)    = "f "  ++ show op
  show (OFloatOp op)   = "of " ++ show op
  show (UFloatOp op)   = "uf " ++ show op

instance Show ArithOpType where
  show Add       = "+"
  show SatAdd    = "+"
  show Sub       = "-"
  show SatSub    = "-"
  show Mul       = "*"
  show Div       = "/"
  show Rem       = "%"
  show Shl       = "<<"
  show LShr      = ">>"
  show AShr      = ">>"
  show And       = "&&"
  show Or        = "||"
  show XOr       = "^"
  show Not       = "!"
  show Eq        = "=="
  show NEq       = "!="
  show GT        = ">"
  show GE        = ">="
  show LT        = "<"
  show LE        = "<="
  show Sqrt      = "sqrt"
  show Ordered   = "ord"
  show Unordered = "uno"

instance Show TypeConvOp where
  show ZExt  = "zext"
  show SExt  = "sext"
  show Trunc = "trunc"
  show Float2SInt = "fptosi"
  show Float2UInt = "fptoui"
  show SInt2Float = "sitofp"
  show UInt2Float = "uitofp"

instance Show ControlOp where
  show Br     = "br"
  show CondBr = "cbr"
  show Ret    = "ret"



------------------------------------------
-- DebugShow-related type class instances
------------------------------------------

instance DebugShow CompOp where
  dShow (CompArithOp op) = dShow op
  dShow (CompTypeConvOp op) = dShow op

instance DebugShow ArithOp where
  dShow = show . getArithOpType

instance DebugShow ArithOpType where
  dShow = show

instance DebugShow TypeConvOp where
  dShow = show

instance DebugShow ControlOp where
  dShow CondBr = "cond.br"
  dShow c      = show c



-------------------------------------
-- JSON-related type class instances
-------------------------------------

instance FromJSON CompOp where
  parseJSON (String v) =
    do let str = unpack v
           split = splitOn " " str
       case length split of
         2 -> do let ops = [ Add, SatAdd, Sub, SatSub, Mul, Div, Rem, Shl, LShr
                           , AShr, And, Or, XOr, Not, Eq, NEq, GT, GE, LT, LE
                           , Sqrt, Ordered, Unordered
                           ]
                     [kind, op_str] = split
                     found = filter (\op -> show op == op_str) ops
                 when (null found) mzero
                 let the_op = head found
                     ari_op = case kind of "i"  -> Just $ IntOp the_op
                                           "ui" -> Just $ UIntOp the_op
                                           "si" -> Just $ SIntOp the_op
                                           "f"  -> Just $ FloatOp the_op
                                           "uf" -> Just $ UFloatOp the_op
                                           "of" -> Just $ OFloatOp the_op
                                           _    -> Nothing
                 if isJust ari_op
                 then return $ CompArithOp $ fromJust ari_op
                 else mzero
         1 -> do let ops = [ ZExt, SExt, Trunc
                           , Float2SInt, Float2UInt, SInt2Float, UInt2Float
                           ]
                     found = filter (\op -> show op == str) ops
                 when (null found) mzero
                 return $ CompTypeConvOp $ head found
         _ -> mzero
  parseJSON _ = mzero

instance ToJSON CompOp where
  toJSON op = String $ pack $ show op

instance FromJSON ControlOp where
  parseJSON (String v) =
    do let str = unpack v
           ops = [ Br, CondBr, Ret ]
           found = filter (\op -> show op == str) ops
       when (null found) mzero
       return $ head found
  parseJSON _ = mzero

instance ToJSON ControlOp where
  toJSON op = String $ pack $ show op



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
