--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.DataTypes.Base
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

module Language.InstructionSelection.DataTypes.Base where

import Language.InstructionSelection.Utils (Natural (..))



data DataType

      -- | An integer data type, of a certain number of bits.

    = IntType Natural

      -- TODO: add missing data types

    deriving (Show, Eq)
