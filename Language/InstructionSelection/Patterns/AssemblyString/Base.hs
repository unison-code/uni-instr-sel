--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Patterns.AssemblyString.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
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



-- | Record for containing the assembly string to produce during code emission.

data AssemblyString
    = AssemblyString
          -- | TODO: refactor into something that is easier to process.
          String
    deriving (Show)
