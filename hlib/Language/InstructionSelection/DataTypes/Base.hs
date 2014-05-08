--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.DataTypes.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains data types for operations.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.DataTypes.Base where

import Language.InstructionSelection.Utils (Natural (..), toNatural)
import Language.InstructionSelection.PrettyPrint



--------------
-- Data types
--------------

data DataType

      -- | An integer data type, of a certain number of bits.

    = IntType Natural

      -- TODO: add missing data types

      -- | When the data type is unknown and does not matter.

    | AnyType

    deriving (Show, Eq)



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



------------------------
-- Type class instances
------------------------

instance PrettyPrint DataType where
  prettyShow (IntType w) = "i" ++ show w
  prettyShow AnyType = ""
