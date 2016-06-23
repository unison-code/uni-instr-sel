{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

Contributing authors:
  Roberto Castaneda Lozano <rcas@sics.se>

-}

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstrSel.OpTypes.Base where

import Language.InstrSel.PrettyShow
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
  | CompMemoryOp MemoryOp
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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
  deriving (Show, Eq)

-- | Operations that define or use values by accessing memory. All operations
-- take at least two operands - the first operand is the state, and the second
-- operand is the address - and produce at least one output, which is a new
-- state.
data MemoryOp
    -- | Read from memory.
  = Load
    -- | Store to memory.
  | Store
  deriving (Show, Eq)

data ControlOp
    -- | Unconditional branch (same as a jump).
  = Br
    -- | Conditional branch. Branching is done if the input value is not zero.
    -- The first operand is the true branch, and the second operand is the false
    -- branch.
  | CondBr
    -- | Return.
  | Ret
  deriving (Show, Eq)



-------------------------------------
-- PrettyShow-related type class instances
-------------------------------------

instance PrettyShow CompOp where
  pShow (CompArithOp op)    = pShow op
  pShow (CompTypeConvOp op) = pShow op
  pShow (CompMemoryOp op)   = pShow op

instance PrettyShow ArithOp where
  pShow (IntOp op)      = "i "  ++ pShow op
  pShow (UIntOp op)     = "ui " ++ pShow op
  pShow (SIntOp op)     = "si " ++ pShow op
  pShow (FixpointOp op) = "fix "++ pShow op
  pShow (FloatOp op)    = "f "  ++ pShow op
  pShow (OFloatOp op)   = "of " ++ pShow op
  pShow (UFloatOp op)   = "uf " ++ pShow op

instance PrettyShow ArithOpType where
  pShow Add       = "+"
  pShow SatAdd    = "+"
  pShow Sub       = "-"
  pShow SatSub    = "-"
  pShow Mul       = "*"
  pShow Div       = "/"
  pShow Rem       = "%"
  pShow Shl       = "<<"
  pShow LShr      = ">>"
  pShow AShr      = "a>>"
  pShow And       = "&&"
  pShow Or        = "||"
  pShow XOr       = "^"
  pShow Not       = "!"
  pShow Eq        = "=="
  pShow NEq       = "!="
  pShow GT        = ">"
  pShow GE        = ">="
  pShow LT        = "<"
  pShow LE        = "<="
  pShow Sqrt      = "sqrt"
  pShow Ordered   = "ord"
  pShow Unordered = "uno"

instance PrettyShow TypeConvOp where
  pShow ZExt  = "zext"
  pShow SExt  = "sext"
  pShow Trunc = "trunc"
  pShow Float2SInt = "fptosi"
  pShow Float2UInt = "fptoui"
  pShow SInt2Float = "sitofp"
  pShow UInt2Float = "uitofp"

instance PrettyShow MemoryOp where
  pShow Load  = "load"
  pShow Store = "store"

instance PrettyShow ControlOp where
  pShow Br     = "br"
  pShow CondBr = "cbr"
  pShow Ret    = "ret"



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
                     found = filter (\op -> pShow op == op_str) ops
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
         1 -> do let tcops   = [ ZExt, SExt, Trunc
                               , Float2SInt, Float2UInt, SInt2Float, UInt2Float
                               ]
                     mops    = [ Load, Store ]
                     tcfound = filter (\op -> pShow op == str) tcops
                     mfound  = filter (\op -> pShow op == str) mops
                 when (null tcfound && null mfound) mzero
                 case (null tcfound, null mfound) of
                   (False, True) -> return $ CompTypeConvOp $ head tcfound
                   (True, False) -> return $ CompMemoryOp $ head mfound
                   _ -> mzero
         _ -> mzero
  parseJSON _ = mzero

instance ToJSON CompOp where
  toJSON op = String $ pack $ pShow op

instance FromJSON ControlOp where
  parseJSON (String v) =
    do let str = unpack v
           ops = [ Br, CondBr, Ret ]
           found = filter (\op -> pShow op == str) ops
       when (null found) mzero
       return $ head found
  parseJSON _ = mzero

instance ToJSON ControlOp where
  toJSON op = String $ pack $ pShow op



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
isOpCommutative (CompMemoryOp Load) = True
isOpCommutative (CompMemoryOp Store) = False

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
numOperandsForCompOp (CompMemoryOp Load) = 1
numOperandsForCompOp (CompMemoryOp Store) = 2

-- | Gets the number of operands required by a given arithmetic operation.
numOperandsForArithOp :: ArithOp -> Natural
numOperandsForArithOp = numOperandsForArithOpType . getArithOpType

-- | Gets the number of operands required by a given arithmetic operation type.
numOperandsForArithOpType :: ArithOpType -> Natural
numOperandsForArithOpType op
  | op `elem` [ Not, Sqrt ] = 1
  | otherwise = 2

-- | Checks if a computation is compatible with another, meaning that they are
-- semantically equivalent. Note that this function is not necessarily
-- commutative.
isCompOpCompatibleWith :: CompOp -> CompOp -> Bool
isCompOpCompatibleWith (CompArithOp op1) (CompArithOp op2) =
  op1 `isArithOpCompatibleWith` op2
isCompOpCompatibleWith (CompTypeConvOp op1) (CompTypeConvOp op2) =
  op1 `isTypeConvOpCompatibleWith` op2
isCompOpCompatibleWith (CompMemoryOp op1) (CompMemoryOp op2) =
  op1 `isMemoryOpCompatibleWith` op2
isCompOpCompatibleWith _ _ = False

-- | Checks if an arithmetic operation is compatible another operation, meaning
-- that they are semantically equivalent. Note that this function is not
-- necessarily commutative.
isArithOpCompatibleWith :: ArithOp -> ArithOp -> Bool
isArithOpCompatibleWith  ( IntOp op1)    ( IntOp op2) = op1 == op2
isArithOpCompatibleWith  ( IntOp op1)    (UIntOp op2) = op1 == op2
isArithOpCompatibleWith  ( IntOp op1)    (SIntOp op2) = op1 == op2
isArithOpCompatibleWith  (UIntOp op1)    ( IntOp op2) = op1 == op2
isArithOpCompatibleWith  (SIntOp op1)    ( IntOp op2) = op1 == op2
isArithOpCompatibleWith ( FloatOp op1) ( FloatOp op2) = op1 == op2
isArithOpCompatibleWith ( FloatOp op1) (OFloatOp op2) = op1 == op2
isArithOpCompatibleWith ( FloatOp op1) (UFloatOp op2) = op1 == op2
isArithOpCompatibleWith (UFloatOp op1) ( FloatOp op2) = op1 == op2
isArithOpCompatibleWith (OFloatOp op1) ( FloatOp op2) = op1 == op2
isArithOpCompatibleWith op1 op2                       = op1 == op2

-- | Checks if a type conversion operation is compatible with another operation,
-- meaning that they are semantically equivalent. Note that this function is not
-- necessarily commutative.
isTypeConvOpCompatibleWith :: TypeConvOp -> TypeConvOp -> Bool
isTypeConvOpCompatibleWith = (==)

-- | Checks if a memory operation is compatible with another operation, meaning
-- that they are semantically equivalent. Note that this function is not
-- necessarily commutative.
isMemoryOpCompatibleWith :: MemoryOp -> MemoryOp -> Bool
isMemoryOpCompatibleWith = (==)

-- | Updates the operation type inside an 'ArithOp'.
updateArithOpType :: ArithOp -> ArithOpType -> ArithOp
updateArithOpType ( IntOp _)     op =  IntOp op
updateArithOpType (UIntOp _)     op = UIntOp op
updateArithOpType (SIntOp _)     op = SIntOp op
updateArithOpType (FixpointOp _) op = FixpointOp op
updateArithOpType ( FloatOp _)   op =  FloatOp op
updateArithOpType (OFloatOp _)   op = OFloatOp op
updateArithOpType (UFloatOp _)   op = UFloatOp op

-- | Inverts a given comparision. For example, '>' returns '<='.
invertArithComparison :: ArithOp -> ArithOp
invertArithComparison op =
  let op_type = getArithOpType op
  in updateArithOpType op (invertArithOpComparisonType op_type)

-- | Inverts a given comparision. For example, '>' returns '<='.
invertArithOpComparisonType :: ArithOpType -> ArithOpType
invertArithOpComparisonType Eq  = NEq
invertArithOpComparisonType NEq = Eq
invertArithOpComparisonType GT  = LE
invertArithOpComparisonType GE  = LT
invertArithOpComparisonType LT  = GE
invertArithOpComparisonType LE  = GT
invertArithOpComparisonType op =
  error $ "invertArithOpComparisonType: cannot invert " ++ show op
          ++ " because it is not a comparison"

-- | Swaps a given comparision. For example, '>' returns '<'.
swapComparison :: CompOp -> CompOp
swapComparison (CompArithOp op) = CompArithOp $ swapArithComparison op
swapComparison op =
  error $ "swapComparison: cannot swap " ++ show op
          ++ " because it is not an arithmetic operation"

-- | Swaps a given comparision. For example, '>' returns '<'.
swapArithComparison :: ArithOp -> ArithOp
swapArithComparison op =
  let op_type = getArithOpType op
  in updateArithOpType op (swapArithOpComparisonType op_type)

-- | Swaps a given comparision. For example, '>' returns '<='.
swapArithOpComparisonType :: ArithOpType -> ArithOpType
swapArithOpComparisonType Eq  = Eq
swapArithOpComparisonType NEq = NEq
swapArithOpComparisonType GT  = LT
swapArithOpComparisonType GE  = LE
swapArithOpComparisonType LT  = GT
swapArithOpComparisonType LE  = GE
swapArithOpComparisonType op =
  error $ "swapArithOpComparisonType: cannot swap " ++ show op
          ++ " because it is not a comparison"
