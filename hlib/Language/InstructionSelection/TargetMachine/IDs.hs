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

module Language.InstructionSelection.TargetMachine.IDs
  ( AssemblyID (..)
  , BBLabelID (..)
  , InstructionID (..)
  , RegisterID (..)
  , TargetMachineID (..)
  , fromAssemblyID
  , fromInstructionID
  , fromRegisterID
  , fromTargetMachineID
  , toAssemblyID
  , toInstructionID
  , toRegisterID
  , toTargetMachineID
  )
where

import Language.InstructionSelection.Utils
  ( Natural
  , toNatural
  )



--------------
-- Data types
--------------

-- | Represents a basic block label identifier.

newtype BBLabelID
    = BBLabelID String
    deriving (Eq)

instance Show BBLabelID where
  show (BBLabelID str) = show str

-- | Represents an instruction ID.

newtype InstructionID = InstructionID Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show InstructionID where
  show (InstructionID i) = show i

-- | Represents an ID to be used as place-holders inside an assembly string.

newtype AssemblyID = AssemblyID Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show AssemblyID where
  show (AssemblyID i) = show i

-- | Represents a register ID.

newtype RegisterID = RegisterID Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show RegisterID where
  show (RegisterID i) = show i

-- | Represents a target machine ID

newtype TargetMachineID = TargetMachineID String
  deriving (Eq, Show)



-------------
-- Functions
-------------

fromRegisterID :: RegisterID -> Natural
fromRegisterID (RegisterID i) = i

toRegisterID :: (Integral i) => i -> RegisterID
toRegisterID = RegisterID . toNatural

fromTargetMachineID :: TargetMachineID -> String
fromTargetMachineID (TargetMachineID i) = i

toTargetMachineID :: String -> TargetMachineID
toTargetMachineID = TargetMachineID

fromInstructionID :: InstructionID -> Natural
fromInstructionID (InstructionID i) = i

toInstructionID :: (Integral i) => i -> InstructionID
toInstructionID = InstructionID . toNatural

fromAssemblyID :: AssemblyID -> Natural
fromAssemblyID (AssemblyID i) = i

toAssemblyID :: (Integral i) => i -> AssemblyID
toAssemblyID = AssemblyID . toNatural
