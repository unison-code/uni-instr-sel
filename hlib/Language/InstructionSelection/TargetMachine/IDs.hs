--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.TargetMachine.IDs
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains the data types for representing various IDs.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.InstructionSelection.TargetMachine.IDs (
  RegisterID (..)
, fromRegisterID
, toRegisterID
) where

import Language.InstructionSelection.Utils
  ( Natural
  , toNatural
  )



--------------
-- Data types
--------------

-- | Represents a register ID.

newtype RegisterID = RegisterID Natural
  deriving (Eq, Ord, Num, Enum)

instance Show RegisterID where
  show (RegisterID i) = show i



-------------
-- Functions
-------------

fromRegisterID :: RegisterID -> Natural
fromRegisterID (RegisterID i) = i

toRegisterID :: (Integral i) => i -> RegisterID
toRegisterID = RegisterID . toNatural
