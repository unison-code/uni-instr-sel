{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstrSel.DataTypes.Base
  ( DataType (..)
  , getTypeBitWidth
  , isIntTempType
  , isIntTempTypeAnyWidth
  , isIntConstType
  , isPointerTempType
  , isPointerNullType
  , isPointerConstType
  , isAnyType
  , isVoidType
  , isTypeAConstValue
  , isTypeAPointer
  , isCompatibleWith
  , hasSameBitWidth
  , parseDataTypeFromJson
  , isSingletonConstant
  , areSameConstants
  )
where

import Language.InstrSel.PrettyShow
import Language.InstrSel.Utils.JSON
import Language.InstrSel.Utils.Natural
import Language.InstrSel.Utils.Range
import Language.InstrSel.Utils.String
  ( maybeRead
  , splitOn
  )

import Data.Maybe
  ( catMaybes
  , fromJust
  , isJust
  , isNothing
  )



--------------
-- Data types
--------------

-- | Data type representations.
data DataType
    -- | Represents an integer value stored in a temporary.
  = IntTempType
      { intTempNumBits :: Natural
        -- ^ Number of bits required for the temporary.
      }
    -- | Represents an integer value stored in a temporary of arbitrary bit
    -- width. This type is only to be used in the null instruction for copying
    -- temporaries.
  | IntTempTypeAnyWidth
    -- | Represents an integer constant.
  | IntConstType
      { intConstValue :: Range Integer
        -- ^ The value range of the constant. If the lower and upper bound of
        -- the range are the same, then the 'IntConstType' represents a single
        -- value.
      , intConstNumBits :: Maybe Natural
        -- ^ Number of bits required for the constant, if this is known (the
        -- width is only necessary to be able to perform copy extension of the
        -- program graph).
      }
    -- | Represents a pointer value stored in a temporary.
  | PointerTempType
    -- | Represents a null pointer value.
  | PointerNullType
    -- | Represents a pointer constant.
  | PointerConstType
    -- | When there is no data type.
  | VoidType
    -- | When the data type does not matter.
  | AnyType
  deriving (Show, Eq)



-------------------------------------
-- PrettyShow-related type class instances
-------------------------------------

instance PrettyShow DataType where
  pShow d@(IntTempType {}) = "i" ++ pShow (intTempNumBits d)
  pShow IntTempTypeAnyWidth = "i?"
  pShow d@(IntConstType {}) =
    let b = intConstNumBits d
    in pShow (intConstValue d) ++
       if isJust b then "#i" ++ (pShow $ fromJust b) else ""
  pShow PointerTempType = "ptr"
  pShow PointerNullType = "null"
  pShow PointerConstType = "cnstptr"
  pShow VoidType = "void"
  pShow AnyType = "any"



-------------------------------------
-- JSON-related type class instances
-------------------------------------

instance FromJSON DataType where
  parseJSON (String v) =
    do let str = unpack v
           mt = parseDataTypeFromJson str
       when (isNothing mt) mzero
       return $ fromJust mt
  parseJSON _ = mzero

instance ToJSON DataType where
  toJSON t = String $ pack $ pShow t



-------------
-- Functions
-------------

-- | Checks if a given data type is 'IntTempType'.
isIntTempType :: DataType -> Bool
isIntTempType IntTempType {} = True
isIntTempType _ = False

-- | Checks if a given data type is 'IntTempTypeAnyWidth'.
isIntTempTypeAnyWidth :: DataType -> Bool
isIntTempTypeAnyWidth IntTempTypeAnyWidth = True
isIntTempTypeAnyWidth _ = False

-- | Checks if a given data type is 'IntConstType'.
isIntConstType :: DataType -> Bool
isIntConstType IntConstType {} = True
isIntConstType _ = False

-- | Checks if a given data type is 'PointerTempType'.
isPointerTempType :: DataType -> Bool
isPointerTempType PointerTempType = True
isPointerTempType _ = False

-- | Checks if a given data type is 'PointerNullType'.
isPointerNullType :: DataType -> Bool
isPointerNullType PointerNullType = True
isPointerNullType _ = False

-- | Checks if a given data type is 'PointerConstType'.
isPointerConstType :: DataType -> Bool
isPointerConstType PointerConstType = True
isPointerConstType _ = False

-- | Checks if a given data type is 'VoidType'.
isVoidType :: DataType -> Bool
isVoidType VoidType = True
isVoidType _ = False

-- | Checks if a given data type is 'AnyType'.
isAnyType :: DataType -> Bool
isAnyType AnyType = True
isAnyType _ = False

-- | Checks if a data type is compatible with another data type. Note that this
-- function is not necessarily commutative.
isCompatibleWith :: DataType -> DataType -> Bool
isCompatibleWith d1@(IntTempType {}) d2@(IntTempType {}) =
  intTempNumBits d1 == intTempNumBits d2
isCompatibleWith IntTempTypeAnyWidth (IntTempType {}) = True
isCompatibleWith d1@(IntConstType {}) d2@(IntConstType {}) =
  (intConstValue d1) `contains` (intConstValue d2)
isCompatibleWith PointerTempType PointerTempType = True
isCompatibleWith PointerNullType PointerNullType = True
isCompatibleWith PointerConstType PointerConstType = True
isCompatibleWith VoidType VoidType = True
isCompatibleWith AnyType _ = True
isCompatibleWith _ _ = False

-- | Gets the bit width of a given data type. If the data type has no bit width,
-- 'Nothing' is returned.
getTypeBitWidth :: DataType -> Maybe Natural
getTypeBitWidth (IntTempType w) = Just w
getTypeBitWidth (IntConstType { intConstNumBits = Just w }) = Just w
getTypeBitWidth _ = Nothing

-- | Checks if two data types have the same bit width.
hasSameBitWidth :: DataType -> DataType -> Bool
hasSameBitWidth d1@(IntTempType {}) d2@(IntTempType {}) =
  intTempNumBits d1 == intTempNumBits d2
hasSameBitWidth d1@(IntTempType {}) d2@(IntConstType {}) =
  if (isJust $ intConstNumBits d2)
  then intTempNumBits d1 == (fromJust $ intConstNumBits d2)
  else False
hasSameBitWidth d1@(IntConstType {}) d2@(IntTempType {}) =
  if (isJust $ intConstNumBits d1)
  then (fromJust $ intConstNumBits d1) == intTempNumBits d2
  else False
hasSameBitWidth d1@(IntConstType {}) d2@(IntConstType {}) =
  if (isJust $ intConstNumBits d1) && (isJust $ intConstNumBits d2)
  then (fromJust $ intConstNumBits d1) == (fromJust $ intConstNumBits d2)
  else False
hasSameBitWidth _ _ = False

-- | Parses a data type from a JSON string. If parsing fails, 'Nothing' is
-- returned.
parseDataTypeFromJson :: String -> Maybe DataType
parseDataTypeFromJson str =
  let res = catMaybes [ parseIntTempTypeFromJson str
                      , parseIntTempTypeAnyWidthFromJson str
                      , parseIntConstTypeFromJson str
                      , parsePointerTempTypeFromJson str
                      , parsePointerNullTypeFromJson str
                      , parsePointerConstTypeFromJson str
                      , parseVoidTypeFromJson str
                      , parseAnyTypeFromJson str
                      ]
  in if length res > 0
     then Just $ head res
     else Nothing

-- | Parses an 'IntTempType' from a JSON string. If parsing fails, 'Nothing' is
-- returned.
parseIntTempTypeFromJson :: String -> Maybe DataType
parseIntTempTypeFromJson str =
  if head str == 'i'
  then let numbits = do int <- maybeRead (tail str) :: Maybe Integer
                        maybeToNatural int
       in if isJust numbits
          then Just $ IntTempType { intTempNumBits = fromJust numbits }
          else Nothing
  else Nothing

-- | Parses an 'IntTempTypeAnyWidth' from a JSON string. If parsing fails,
-- 'Nothing' is returned.
parseIntTempTypeAnyWidthFromJson :: String -> Maybe DataType
parseIntTempTypeAnyWidthFromJson str =
  if str == pShow IntTempTypeAnyWidth
  then Just IntTempTypeAnyWidth
  else Nothing

-- | Parses an 'IntConstType' from a JSON string. If parsing fails, 'Nothing' is
-- returned.
parseIntConstTypeFromJson :: String -> Maybe DataType
parseIntConstTypeFromJson str =
  let parts = splitOn "#" str
  in if length parts == 1 || length parts == 2
     then let value = parseRangeStr (head parts)
              numbits_str = last parts
              numbits = if length parts == 2 && (head numbits_str == 'i')
                        then do int <- maybeRead (tail numbits_str) :: Maybe
                                                                       Integer
                                maybeToNatural int
                        else Nothing
          in if isJust value && (not (length parts == 2) || isJust numbits)
             then Just $ IntConstType { intConstValue = fromJust value
                                      , intConstNumBits = numbits
                                      }
             else Nothing
     else Nothing

-- | Parses a 'VoidType' from a JSON string. If parsing fails, 'Nothing' is
-- returned.
parseVoidTypeFromJson :: String -> Maybe DataType
parseVoidTypeFromJson str =
  if str == pShow VoidType
  then Just VoidType
  else Nothing

-- | Parses an 'AnyType' from a JSON string. If parsing fails, 'Nothing' is
-- returned.
parseAnyTypeFromJson :: String -> Maybe DataType
parseAnyTypeFromJson str =
  if str == pShow AnyType
  then Just AnyType
  else Nothing

-- | Parses a 'PointerTempType' from a JSON string. If parsing fails, 'Nothing'
-- is returned.
parsePointerTempTypeFromJson :: String -> Maybe DataType
parsePointerTempTypeFromJson str =
  if str == pShow PointerTempType
  then Just PointerTempType
  else Nothing

-- | Parses a 'PointerNullType' from a JSON string. If parsing fails, 'Nothing'
-- is returned.
parsePointerNullTypeFromJson :: String -> Maybe DataType
parsePointerNullTypeFromJson str =
  if str == pShow PointerNullType
  then Just PointerNullType
  else Nothing

-- | Parses a 'PointerConstType' from a JSON string. If parsing fails, 'Nothing'
-- is returned.
parsePointerConstTypeFromJson :: String -> Maybe DataType
parsePointerConstTypeFromJson str =
  if str == pShow PointerConstType
  then Just PointerConstType
  else Nothing

-- | Checks if a given data type represents a constant value.
isTypeAConstValue :: DataType -> Bool
isTypeAConstValue t = isIntConstType t ||
                      isPointerNullType t ||
                      isPointerConstType t

-- | Checks if a given data type represents a pointer.
isTypeAPointer :: DataType -> Bool
isTypeAPointer t = isPointerTempType t ||
                   isPointerNullType t ||
                   isPointerConstType t

-- | Checks if two data types represent the same (non-range) constant. Bit
-- widths, if available, are not taken into consideration.
areSameConstants :: DataType -> DataType -> Bool
areSameConstants d1 d2 =
  isSingletonConstant d1 &&
  isSingletonConstant d2 &&
  d1 `matches` d2
  where
  matches (IntConstType {}) (IntConstType {}) =
    (intConstValue d1) == (intConstValue d2)
  matches _ _ = False

-- | Checks if a data type represents a constant with a singleton range.
isSingletonConstant :: DataType -> Bool
isSingletonConstant (IntConstType r _) = isRangeSingleton r
isSingletonConstant PointerNullType = True
isSingletonConstant _ = False
