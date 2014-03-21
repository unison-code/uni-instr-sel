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
-- Contains the data types and records for representing patterns. This is the
-- format on which subsequent preparation for instruction selection will build
-- on (i.e. other pattern forms, such as those based on LLVM, will be converted
-- into this format).
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Patterns.Base (
  InstanceId (..)
, Instruction (..)
, InstProperties (..)
, InstructionId (..)
, PatternId (..)
) where

import Language.InstructionSelection.OpStructures
import Language.InstructionSelection.Patterns.AssemblyString
import Language.InstructionSelection.Utils ( Natural
                                           , toNatural
                                           )



--------------
-- Data types
--------------

-- | Represents an instruction ID.

newtype InstructionId = InstructionId Natural
  deriving (Show, Eq)

instance Num InstructionId where
    fromInteger = InstructionId . toNatural
    (InstructionId x) + (InstructionId y) = InstructionId (x + y)
    (InstructionId x) - (InstructionId y) = InstructionId (x - y)
    (InstructionId x) * (InstructionId y) = InstructionId (x * y)
    abs (InstructionId x) = InstructionId (abs x)
    signum (InstructionId x) = InstructionId (signum x)

instance Enum InstructionId where
  toEnum = InstructionId . toEnum
  fromEnum (InstructionId x) = fromEnum x

-- | Represents a pattern ID. Pattern IDs are used to distinguish which
-- instruction a pattern belongs to. Note, however, that an instance of a
-- pattern - which is an occurrance where a pattern has been matched over a set
-- of nodes in a function graph - is not given pattern IDs but instance IDs.

newtype PatternId = PatternId Natural
  deriving (Show, Eq)

instance Num PatternId where
    fromInteger = PatternId . toNatural
    (PatternId x) + (PatternId y) = PatternId (x + y)
    (PatternId x) - (PatternId y) = PatternId (x - y)
    (PatternId x) * (PatternId y) = PatternId (x * y)
    abs (PatternId x) = PatternId (abs x)
    signum (PatternId x) = PatternId (signum x)

instance Enum PatternId where
  toEnum = PatternId . toEnum
  fromEnum (PatternId x) = fromEnum x

-- | Represents a pattern instance ID. Instance IDs are used to distinguish
-- between pattern an instance is based on.

newtype InstanceId = InstanceId Natural
  deriving (Show, Eq)

instance Num InstanceId where
    fromInteger = InstanceId . toNatural
    (InstanceId x) + (InstanceId y) = InstanceId (x + y)
    (InstanceId x) - (InstanceId y) = InstanceId (x - y)
    (InstanceId x) * (InstanceId y) = InstanceId (x * y)
    abs (InstanceId x) = InstanceId (abs x)
    signum (InstanceId x) = InstanceId (signum x)

instance Enum InstanceId where
  toEnum = InstanceId . toEnum
  fromEnum (InstanceId x) = fromEnum x

-- | TODO: write description

data Instruction
    = Instruction {

          -- | An ID which is globally unique across all instructions, but not
          -- necessarily contiguous.

          instrId :: InstructionId

          -- | Patterns which correspond to the instruction. There must be at
          -- least one pattern. Each pattern also has a corresponding ID which
          -- must be globally unique across all patterns and all instructions,
          -- but not necessarily contiguous.

        , patterns :: [(OpStructure, PatternId)]

          -- | Instruction properties.

        , instProps :: InstProperties

          -- | Assembly string to produce upon code emission.

        , assemblyStr :: AssemblyString

      }
    deriving (Show)

-- | TODO: write description

data InstProperties
    = InstProperties {

          -- | Instruction code size (in bytes).

          codeSize :: Integer

          -- | Instruction latency (in cycles).

        , latency :: Integer

      }
    deriving (Show)
