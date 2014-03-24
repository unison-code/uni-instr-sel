--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Machine.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data types and records for representing a target machine.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.InstructionSelection.Machine.Base (
  RegisterId (..)
, fromRegisterId
, toRegisterId
) where

import Language.InstructionSelection.Utils ( Natural
                                           , toNatural
                                           )



--------------
-- Data types
--------------

-- | Represents a register ID.

newtype RegisterId = RegisterId Natural
  deriving (Show, Eq, Ord, Num, Enum)



-------------
-- Functions
-------------

fromRegisterId :: RegisterId -> Natural
fromRegisterId (RegisterId i) = i

toRegisterId :: (Integral i) => i -> RegisterId
toRegisterId = RegisterId . toNatural
