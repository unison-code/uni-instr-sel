{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
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
import Language.InstrSel.Utils.JSON
import Data.Maybe
  ( fromJust
  , isJust
  )
import Language.InstrSel.Utils.String
  ( splitOn )

import Prelude
  hiding
  ( GT
  , LT
  )



----------------
-- Type classes
----------------

-- | A class for providing useful information about an operation.
class OpType a where
  -- | Gets the number of values that this operation takes as input.
  numOperands :: a -> Int

  -- | Whether this operation is commutative. If the operation takes less than
  -- two operands, then 'True' is returned by default.
  isCommutative :: a -> Bool
  isCommutative a =
    if numOperands a == 1 then True else error "isCommutative: undefined"

  -- | Whether this operation produces a value.
  producesValue :: a -> Bool

  -- | Whether this operation makes use of state.
  requiresState :: a -> Bool

  -- | Checks if an operation is compatible with another, meaning that they are
  -- semantically equivalent. Note that this function is not necessarily
  -- commutative.
  isCompatibleWith :: a -> a -> Bool



--------------
-- Data types
--------------

-- | Computational operations.
data CompOp
  = CompArithOp ArithOp
  | CompTypeConvOp TypeConvOp
  | CompTypeCastOp TypeCastOp
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

-- | Operations that convert values of one type to another type. Unlike
-- 'TypeCastOp', these operations may result in actual code.
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
    -- | Integer to pointer.
  | IntToPtr
    -- | Pointer to integer.
  | PtrToInt
  deriving (Show, Eq)

-- | Operations that cast values of one type to another type. Unlike
-- 'TypeConvOp', these operations never result in actual code.
data TypeCastOp
  = BitCast
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

-- | Operations that exhibit a change in control flow.
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



---------------------------------------
-- OpType-related type class instances
---------------------------------------

instance OpType CompOp where
  numOperands (CompArithOp op) = numOperands op
  numOperands (CompTypeConvOp op) = numOperands op
  numOperands (CompTypeCastOp op) = numOperands op
  numOperands (CompMemoryOp op) = numOperands op

  isCommutative (CompArithOp op) = isCommutative op
  isCommutative (CompTypeConvOp op) = isCommutative op
  isCommutative (CompTypeCastOp op) = isCommutative op
  isCommutative (CompMemoryOp op) = isCommutative op

  producesValue (CompArithOp op) = producesValue op
  producesValue (CompTypeConvOp op) = producesValue op
  producesValue (CompTypeCastOp op) = producesValue op
  producesValue (CompMemoryOp op) = producesValue op

  requiresState (CompArithOp op) = requiresState op
  requiresState (CompTypeConvOp op) = requiresState op
  requiresState (CompTypeCastOp op) = requiresState op
  requiresState (CompMemoryOp op) = requiresState op

  isCompatibleWith (CompArithOp op1) (CompArithOp op2) =
    isCompatibleWith op1 op2
  isCompatibleWith (CompTypeConvOp op1) (CompTypeConvOp op2) =
    isCompatibleWith op1 op2
  isCompatibleWith (CompTypeCastOp op1) (CompTypeCastOp op2) =
    isCompatibleWith op1 op2
  isCompatibleWith (CompMemoryOp op1) (CompMemoryOp op2) =
    isCompatibleWith op1 op2
  isCompatibleWith _ _ = False

instance OpType ArithOp where
  numOperands = numOperands . getArithOpType
  isCommutative = isCommutative . getArithOpType
  producesValue = producesValue . getArithOpType
  requiresState = requiresState . getArithOpType

  isCompatibleWith  ( IntOp op1)    ( IntOp op2) = isCompatibleWith op1 op2
  isCompatibleWith  ( IntOp op1)    (UIntOp op2) = isCompatibleWith op1 op2
  isCompatibleWith  ( IntOp op1)    (SIntOp op2) = isCompatibleWith op1 op2
  isCompatibleWith  (UIntOp op1)    (UIntOp op2) = isCompatibleWith op1 op2
  isCompatibleWith  (SIntOp op1)    (SIntOp op2) = isCompatibleWith op1 op2
  isCompatibleWith ( FloatOp op1) ( FloatOp op2) = isCompatibleWith op1 op2
  isCompatibleWith ( FloatOp op1) (OFloatOp op2) = isCompatibleWith op1 op2
  isCompatibleWith ( FloatOp op1) (UFloatOp op2) = isCompatibleWith op1 op2
  isCompatibleWith (OFloatOp op1) (OFloatOp op2) = isCompatibleWith op1 op2
  isCompatibleWith (UFloatOp op1) (UFloatOp op2) = isCompatibleWith op1 op2
  isCompatibleWith _ _ = False

instance OpType ArithOpType where
  numOperands op
    | op `elem` [ Not, Sqrt ] = 1
    | otherwise = 2
  isCommutative op =
    op `notElem` [ Sub, SatSub, Div, Rem, Shl, LShr, AShr, GT, GE, LT, LE ]
  producesValue _ = True
  requiresState _ = False
  isCompatibleWith = (==)

instance OpType TypeConvOp where
  numOperands _ = 1
  producesValue _ = True
  requiresState _ = False
  isCompatibleWith = (==)

instance OpType TypeCastOp where
  numOperands _ = 1
  producesValue _ = True
  requiresState _ = False
  isCompatibleWith = (==)

instance OpType MemoryOp where
  numOperands Load  = 1
  numOperands Store = 2
  isCommutative Load  = True
  isCommutative Store = False
  producesValue Load  = True
  producesValue Store = False
  requiresState _ = True
  isCompatibleWith = (==)

instance OpType ControlOp where
  numOperands Br  = 0
  numOperands CondBr = 1
  numOperands Ret = error "numOperands: undefined for Ret"
  producesValue _ = False
  requiresState _ = False
  isCompatibleWith = (==)



-------------------------------------------
-- PrettyShow-related type class instances
-------------------------------------------

instance PrettyShow CompOp where
  pShow (CompArithOp op)    = pShow op
  pShow (CompTypeConvOp op) = pShow op
  pShow (CompTypeCastOp op) = pShow op
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
  pShow LShr      = "l>>"
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
  pShow IntToPtr = "int-to-ptr"
  pShow PtrToInt = "ptr-to-int"

instance PrettyShow TypeCastOp where
  pShow BitCast  = "bit-cast"

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
                               , IntToPtr, PtrToInt
                               ]
                     ccops   = [ BitCast ]
                     mops    = [ Load, Store ]
                     tcfound = filter (\op -> pShow op == str) tcops
                     ccfound = filter (\op -> pShow op == str) ccops
                     mfound  = filter (\op -> pShow op == str) mops
                 case (null tcfound, null ccfound, null mfound) of
                   (False, True, True) -> return $ CompTypeConvOp $ head tcfound
                   (True, False, True) -> return $ CompTypeCastOp $ head ccfound
                   (True, True, False) -> return $ CompMemoryOp $ head mfound
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

-- | Checks if a 'CompOp' is of type 'CompArithOp'.
isCompArithOp :: CompOp -> Bool
isCompArithOp (CompArithOp _) = True
isCompArithOp _ = False

-- | Checks if a 'CompOp' is of type 'CompTypeConvOp'.
isCompTypeConvOp :: CompOp -> Bool
isCompTypeConvOp (CompTypeConvOp _) = True
isCompTypeConvOp _ = False

-- | Checks if a 'CompOp' is of type 'CompTypeCastOp'.
isCompTypeCastOp :: CompOp -> Bool
isCompTypeCastOp (CompTypeCastOp _) = True
isCompTypeCastOp _ = False

-- | Checks if a 'CompOp' is of type 'CompMemoryOp'.
isCompMemoryOp :: CompOp -> Bool
isCompMemoryOp (CompMemoryOp _) = True
isCompMemoryOp _ = False

-- | Gets the operation type from an arithmetic operation.
getArithOpType :: ArithOp -> ArithOpType
getArithOpType ( IntOp op)     = op
getArithOpType (UIntOp op)     = op
getArithOpType (SIntOp op)     = op
getArithOpType (FixpointOp op) = op
getArithOpType ( FloatOp op)   = op
getArithOpType (OFloatOp op)   = op
getArithOpType (UFloatOp op)   = op

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
  error $ "invertArithOpComparisonType: cannot invert " ++ show op ++
          " because it is not a comparison"

-- | Swaps a given comparision. For example, '>' returns '<'.
swapComparison :: CompOp -> CompOp
swapComparison (CompArithOp op) = CompArithOp $ swapArithComparison op
swapComparison op =
  error $ "swapComparison: cannot swap " ++ show op ++
          " because it is not an arithmetic operation"

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
  error $ "swapArithOpComparisonType: cannot swap " ++ show op ++
          " because it is not a comparison"
