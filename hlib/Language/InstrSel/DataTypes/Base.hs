--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.DataTypes.Base
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

module Language.InstrSel.DataTypes.Base
  ( DataType (..)
  , isIntTempType
  , isIntConstType
  , isAnyType
  , isTypeAConstValue
  , isDataTypeCompatibleWith
  , parseDataTypeFromJson
  )
where

import Language.InstrSel.DebugShow
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
    -- | Represents an integer constant.
  | IntConstType
      { intConstValue :: Range Integer
        -- ^ The value range of the constant. If it is a singleton, the lower
        -- and upper bound of the range should be the same.
      , intConstNumBits :: Maybe Natural
        -- ^ Number of bits required for the constant, if this is known. This is
        -- really only necessary to be able to perform copy extension of the
        -- program graph.
      }
    -- | When the data type does not matter.
  | AnyType
  deriving Eq



-------------------------------------
-- Show-related type class instances
-------------------------------------

instance Show DataType where
  show d@(IntTempType {}) = "i" ++ show (intTempNumBits d)
  show d@(IntConstType {}) =
    let b = intConstNumBits d
    in show (intConstValue d) ++
       if isJust b then " " ++ (show $ fromJust b) else ""
  show AnyType = "any"



------------------------------------------
-- DebugShow-related type class instances
------------------------------------------

instance DebugShow DataType where
  dShow d@(IntTempType {}) = show d
  dShow d@(IntConstType {}) = show $ intConstValue d
  dShow AnyType = ""



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
  toJSON t = String $ pack $ show t



-------------
-- Functions
-------------

-- | Checks if a given data type is 'IntTempType'.
isIntTempType :: DataType -> Bool
isIntTempType IntTempType {} = True
isIntTempType _ = False

-- | Checks if a given data type is 'IntConstType'.
isIntConstType :: DataType -> Bool
isIntConstType IntConstType {} = True
isIntConstType _ = False

-- | Checks if a given data type is 'AnyType'.
isAnyType :: DataType -> Bool
isAnyType AnyType = True
isAnyType _ = False

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
isDataTypeCompatibleWith d1@(IntConstType {}) d2@(IntConstType {}) =
  (intConstValue d1) `contains` (intConstValue d2)
isDataTypeCompatibleWith AnyType _ = True
isDataTypeCompatibleWith _ AnyType = True
isDataTypeCompatibleWith _ _ = False


-- | Parses a data type from a JSON string. If parsing fails, 'Nothing' is
-- returned.
parseDataTypeFromJson :: String -> Maybe DataType
parseDataTypeFromJson str =
  let res = catMaybes [ parseIntTempTypeFromJson str
                      , parseIntConstTypeFromJson str
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

-- | Parses an 'IntConstType' from a JSON string. If parsing fails, 'Nothing' is
-- returned.
parseIntConstTypeFromJson :: String -> Maybe DataType
parseIntConstTypeFromJson str =
  let parts = splitOn " " str
  in if length parts == 1 || length parts == 2
     then let value = parseRangeStr (head parts)
              numbits = if length parts == 2
                        then do int <- maybeRead (last parts) :: Maybe Integer
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
  if str == show AnyType
  then Just AnyType
  else Nothing

-- | Checks if a given data type represents a constant value.
isTypeAConstValue :: DataType -> Bool
isTypeAConstValue = isIntConstType
