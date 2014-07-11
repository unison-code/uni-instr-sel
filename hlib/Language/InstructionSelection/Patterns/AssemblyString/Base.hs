--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Patterns.AssemblyString.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data types and records for representing the assembly string of
-- an instruction.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Patterns.AssemblyString.Base where

import Language.InstructionSelection.Patterns.IDs (AssemblyID)



--------------
-- Data types
--------------

-- | Represents a basic block label.

newtype BBLabel
    = BBLabel String
    deriving (Eq)

instance Show BBLabel where
  show (BBLabel str) = show str

-- | Represents parts of the assembly string. All 'AssemblyID's used within the
-- same 'AssemblyString' *must* be unique and contiguous!

data AssemblyPart

      -- | Denotes string which is meant to be output verbatim.

    = AssemblyVerbatim String

      -- | Denotes an immediate value.

    | AssemblyImmValue AssemblyID

      -- | Denotes a register.

    | AssemblyRegister AssemblyID

      -- | Denotes a basic block label.

    | AssemblyLabel AssemblyID

      -- | Denotes a temporary.

    | AssemblyTemporary AssemblyID

    deriving (Show)

-- | Record for containing the assembly string to produce during code emission.

data AssemblyString
    = AssemblyString { assStrParts :: [AssemblyPart] }
    deriving (Show)
