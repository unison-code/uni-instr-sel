{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
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
  , isIntTempType
  , isIntTempTypeAnyWidth
  , isIntConstType
  , isPointerTempType
  , isPointerNullType
  , isAnyType
  , isVoidType
  , isTypeAConstValue
  , isDataTypeCompatibleWith
  , parseDataTypeFromJson
  )
where

import Language.InstrSel.PrettyShow
import Language.InstrSel.Utils
  ( maybeRead
  , splitOn
  )
import Language.InstrSel.Utils.Natural
import Language.InstrSel.Utils.Range
import Language.InstrSel.Utils.JSON

import Data.Maybe
  ( catMaybes
  , fromJust
  , isJust
  , isNothing
  )



--------------
-- Data types
--------------

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
    -- | When the data type does not matter.
  | AnyType
    -- | When there is no data type.
  | VoidType
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
       if isJust b then " i" ++ (pShow $ fromJust b) else ""
  pShow PointerTempType = "ptr"
  pShow PointerNullType = "null"
  pShow AnyType = "any"
  pShow VoidType = "void"



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

-- | Checks if a given data type is 'AnyType'.
isAnyType :: DataType -> Bool
isAnyType AnyType = True
isAnyType _ = False

-- | Checks if a given data type is 'VoidType'.
isVoidType :: DataType -> Bool
isVoidType VoidType = True
isVoidType _ = False

-- | Checks if a data type is compatible with another data type. Note that this
-- function is not necessarily commutative.
isDataTypeCompatibleWith
  :: DataType
     -- ^ First type.
  -> DataType
     -- ^ Second type.
  -> Bool
isDataTypeCompatibleWith d1@(IntTempType {}) d2@(IntTempType {}) =
  intTempNumBits d1 == intTempNumBits d2
isDataTypeCompatibleWith IntTempTypeAnyWidth (IntTempType {}) = True
isDataTypeCompatibleWith (IntTempType {}) IntTempTypeAnyWidth = True
isDataTypeCompatibleWith d1@(IntConstType {}) d2@(IntConstType {}) =
  (intConstValue d1) `contains` (intConstValue d2)
isDataTypeCompatibleWith AnyType _ = True
isDataTypeCompatibleWith _ AnyType = True
isDataTypeCompatibleWith PointerTempType PointerTempType = True
isDataTypeCompatibleWith PointerTempType PointerNullType = True
isDataTypeCompatibleWith PointerNullType PointerTempType = True
isDataTypeCompatibleWith PointerNullType PointerNullType = True
isDataTypeCompatibleWith VoidType VoidType = True
isDataTypeCompatibleWith _ _ = False


-- | Parses a data type from a JSON string. If parsing fails, 'Nothing' is
-- returned.
parseDataTypeFromJson :: String -> Maybe DataType
parseDataTypeFromJson str =
  let res = catMaybes [ parseIntTempTypeFromJson str
                      , parseIntTempTypeAnyWidthFromJson str
                      , parseIntConstTypeFromJson str
                      , parseAnyTypeFromJson str
                      , parsePointerTempTypeFromJson str
                      , parsePointerNullTypeFromJson str
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
  let parts = splitOn " " str
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

-- | Checks if a given data type represents a constant value.
isTypeAConstValue :: DataType -> Bool
isTypeAConstValue = isIntConstType
