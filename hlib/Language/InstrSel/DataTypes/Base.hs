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

module Language.InstrSel.DataTypes.Base where

import Language.InstrSel.DebugShow
import Language.InstrSel.Utils
  ( Natural
  , maybeToNatural
  , toNatural
  )
import Language.InstrSel.Utils.JSON

import Data.Maybe
  ( fromJust
  , isNothing
  )



--------------
-- Data types
--------------

data DataType
    -- | An integer data type, of a certain number of bits.
  = IntType Natural
    -- | When the data type is unknown and does not matter.
  | AnyType
  deriving (Eq)



-------------
-- Functions
-------------

-- | Gets the data type of a corresponding integer of a certain number of bits.
fromIWidth :: (Integral a) => a -> DataType
fromIWidth w = IntType $ toNatural w

-- | Gets the data type of a corresponding integer value.
fromIValue :: (Integral a) => a -> DataType
fromIValue 0 = IntType 1
fromIValue i =
  let log2value = logBase 2 $ (fromIntegral $ abs i) :: Float
      numbits = (ceiling log2value) :: Integer
  in IntType $ toNatural numbits

-- | Checks if two data types are compatible, meaning that they are semantically
-- equivalent.
areDataTypesCompatible :: DataType -> DataType -> Bool
areDataTypesCompatible AnyType _ = True
areDataTypesCompatible _ AnyType = True
areDataTypesCompatible d1 d2 = d1 == d2



-------------------------------------
-- Show-related type class instances
-------------------------------------

instance Show DataType where
  show (IntType w) = "i" ++ show w
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
           mt = parseDataType str
       when (isNothing mt) mzero
       return $ fromJust mt
  parseJSON _ = mzero

instance ToJSON DataType where
  toJSON t = String $ pack $ show t



-------------
-- Functions
-------------

-- | Parses a data type from a string. If parsing fails, 'Nothing' is returned.
parseDataType :: String -> Maybe DataType
parseDataType [] = Nothing
parseDataType str =
  if str == show AnyType
  then Just AnyType
  else if head str == 'i'
       then do n <- maybeToNatural $ (read (tail str) :: Integer)
               return $ IntType n
       else Nothing
