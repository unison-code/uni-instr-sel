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
  , fromIntWidth
  , fromIntValue
  , isAnyType
  , isIntType
  , isDataTypeAConstValue
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
  ( fromJust
  , isJust
  , isNothing
  )



--------------
-- Data types
--------------

data DataType
    -- | An integer data type.
  = IntType
      { intNumBits :: Natural
        -- ^ Number of bits.
      , intValue :: Maybe (Range Integer)
        -- ^ The value of the integer, if this is known.
      }
    -- | When the data type is unknown and does not matter.
    -- | An (unknown) integer value.
--  | IntValue { intNumBits :: Natural }
--    -- | A (known) integer constant.
--  | IntConstant { intConst :: Range Integer }
--    -- | When the data type is unknown and does not matter.
  | AnyType



-------------
-- Functions
-------------

-- | Gets the data type of a corresponding integer of a certain number of bits.
fromIntWidth :: (Integral a) => a -> DataType
fromIntWidth w = IntType { intNumBits = toNatural w, intValue = Nothing }

-- | Gets the data type of a corresponding integer value.
fromIntValue :: (Integral a) => a -> DataType
fromIntValue 0 = IntType { intNumBits = 1
                         , intValue = Just $ rangeFromSingleton 0
                         }
fromIntValue i =
  let log2value = logBase 2 $ (fromIntegral $ abs i) :: Float
      numbits = (ceiling log2value) :: Integer
  in IntType { intNumBits = toNatural numbits
             , intValue = Just $ rangeFromSingleton $ toInteger i
             }

-- | Checks if a data type is compatible with another data type. A data type is
-- compatible with another data type if:
--    * both have the same number of bits, and
--    * either none have an integer value, or both has an integer value where
--      the range of the first type contains the range of the second type.
-- Note that this function is not necessarily commutative.
isDataTypeCompatibleWith
  :: DataType
     -- ^ First type.
  -> DataType
     -- ^ Second type.
  -> Bool
isDataTypeCompatibleWith AnyType _ = True
isDataTypeCompatibleWith _ AnyType = True
isDataTypeCompatibleWith d1@(IntType {}) d2@(IntType {}) =
  if intNumBits d1 == intNumBits d2
  then let r1 = intValue d1
           r2 = intValue d2
       in if isNothing r1 && isNothing r2
          then True
          else if isJust r1 && isJust r2
               then (fromJust r1) `contains` (fromJust r2)
               else False
  else False



-------------------------------------
-- Show-related type class instances
-------------------------------------

instance Show DataType where
  show d@(IntType {}) =
    let r = intValue d
    in "i" ++ show (intNumBits d) ++
       if isJust r then " " ++ (show $ fromJust r) else ""
  show AnyType = "any"



------------------------------------------
-- DebugShow-related type class instances
------------------------------------------

instance DebugShow DataType where
  dShow t@(IntType {}) = show t
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

-- | Checks if a given data type is 'AnyType'.
isAnyType :: DataType -> Bool
isAnyType AnyType = True
isAnyType _ = False

-- | Checks if a given data type is 'IntType'.
isIntType :: DataType -> Bool
isIntType IntType {} = True
isIntType _ = False

-- | Parses a data type from a JSON string. If parsing fails, 'Nothing' is
-- returned.
parseDataTypeFromJson :: String -> Maybe DataType
parseDataTypeFromJson str =
  let res1 = parseAnyTypeFromJson str
      res2 = parseIntTypeFromJson str
  in if isJust res1
     then res1
     else if isJust res2
          then res2
          else Nothing

-- | Parses an 'AnyType' from a JSON string. If parsing fails, 'Nothing' is
-- returned.
parseAnyTypeFromJson :: String -> Maybe DataType
parseAnyTypeFromJson str =
  if str == show AnyType
  then Just AnyType
  else Nothing

-- | Parses an 'IntType' from a JSON string. If parsing fails, 'Nothing' is
-- returned.
parseIntTypeFromJson :: String -> Maybe DataType
parseIntTypeFromJson str =
  if head str == 'i'
  then let parts = splitOn " " (tail str)
           numbits = do int <- maybeRead (head parts) :: Maybe Integer
                        maybeToNatural int
           range = parseRangeStr (last parts)
       in if isJust numbits
          then case length parts of
                 1 -> Just $ IntType { intNumBits = fromJust numbits
                                     , intValue = Nothing
                                     }
                 2 -> Just $ IntType { intNumBits = fromJust numbits
                                     , intValue = range
                                     }
                 _ -> Nothing
          else Nothing
  else Nothing

-- | Checks if the given data type represents a constant value.
isDataTypeAConstValue :: DataType -> Bool
isDataTypeAConstValue IntType { intValue = r } =
  isJust r && isRangeSingleton (fromJust r)
isDataTypeAConstValue _ = False
