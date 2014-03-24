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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.InstructionSelection.Patterns.Base (
  InstanceId (..)
, Instruction (..)
, InstProperties (..)
, InstructionId (..)
, PatternId (..)
, fromInstanceId
, fromInstructionId
, fromPatternId
, toInstanceId
, toInstructionId
, toPatternId
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
  deriving (Show, Eq, Ord, Num, Enum)

-- | Represents a pattern ID. Pattern IDs are used to distinguish which
-- instruction a pattern belongs to. Note, however, that an instance of a
-- pattern - which is an occurrance where a pattern has been matched over a set
-- of nodes in a function graph - is not given pattern IDs but instance IDs.

newtype PatternId = PatternId Natural
  deriving (Show, Eq, Ord, Num, Enum)

-- | Represents a pattern instance ID. Instance IDs are used to distinguish
-- between pattern an instance is based on.

newtype InstanceId = InstanceId Natural
  deriving (Show, Eq, Ord, Num, Enum)

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



-------------
-- Functions
-------------

fromInstanceId :: InstanceId -> Natural
fromInstanceId (InstanceId i) = i


toInstanceId :: (Integral i) => i -> InstanceId
toInstanceId = InstanceId . toNatural

fromInstructionId :: InstructionId -> Natural
fromInstructionId (InstructionId i) = i

toInstructionId :: (Integral i) => i -> InstructionId
toInstructionId = InstructionId . toNatural

fromPatternId :: PatternId -> Natural
fromPatternId (PatternId i) = i

toPatternId :: (Integral i) => i -> PatternId
toPatternId = PatternId . toNatural
