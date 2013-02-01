--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Patterns.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Contains the data and types most commonly used for representing the patterns.
-- 
--------------------------------------------------------------------------------

module Language.InstructionSelection.Patterns.Base where

import Language.InstructionSelection.Misc (Range)
import Language.InstructionSelection.Program (Expression)
import Data.Graph.Inductive.Tree (Gr)

-- | Record for describing a data space.

data DataSpace

      -- | Constructor for a register class.

    = RegisterClass {
          regClass :: String
      }

    | MemoryClass {
          memClass :: String

          -- | Address range within the memory class. This may be the entire
          -- range of the memory space, or a restricted subset.

        , addRange :: Range Integer

      }

    deriving (Show, Eq)


-- | Record for describing an immediate.

data Immediate

      -- | Constructor for a named immediate value (i.e. an immediate which is
      -- identified by a symbol but not yet necessarily assigned a value).

    = ImmediateSymbol {
          immId :: String
      }

      -- | Constructor for an assigned immediate value.

    | ImmediateValue {
          immValue :: Integer
      }

    deriving (Show, Eq)

-- | Record for describing an instruction operand.

data InstructionOperand
    = InstructionOperand {
          opId :: String
      }
    deriving (Show, Eq)             


-- | Record for describing a data value. This is simply a wrapper to allow
-- many different types of values.

data DataValue t
    = DataValue {
          value :: t
      }
    deriving (Show, Eq)

















-- | Record for a pattern constraint.

data Constraint

      -- | The @AllocateIn@ constraint dictates that a data value must be
      -- located in a particular space.

    = AllocateIn {
          value :: DataValue
        , space :: DataSpace

      }

      -- | The @ImmediateRange@ constraint limits the range of values that an
      -- immediate value may take.

    | ImmediateRange {
          imm :: Immediate
        , immRange :: Range Integer

      }

      -- | The @Alias@ constraint dictates that two temporaries must be the
      -- same, in the sense that both temporaries must be assigned the same
      -- register.

    | Alias {
          temp1 :: Temporary
        , temp2 :: Temporary

      }

      -- | TODO: add description

    | Assert {
          -- | TODO: refactor into more exact data types
          condition :: String
      }

    deriving (Show)

-- | Record for containing the assembly string to produce during code emission.

data AssemblyString
    = AssemblyString {

          -- | Assembly string. TODO: refactor into something that is easier to
          -- process.

          string :: String

      }
    deriving (Show)

-- | Record for representing a pattern including the constraints.

data Pattern 
    = Pattern {

          -- | The expression DAG which represents the pattern.

          expr :: Expression

          -- | Constraints that need to hold for the pattern.

        , constraints :: [Constraint]

      }
    deriving (Show)

-- | Record for representing a tile. Tiles are the core element of instruction
-- selection.

data Tile

      -- | Constructor for a tile which represents an instruction.

    = Instruction {

          -- | Assembly string produce upon code emission.

          assembly :: AssemblyString

          -- | Patterns which correspond to the instruction. There must be at
          -- least one pattern.

        , patterns :: [Pattern]

      }

    deriving (Show)
