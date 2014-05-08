--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.Patterns.Ids
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

module Language.InstructionSelection.Patterns.Ids (
  InstanceId (..)
, InstructionId (..)
, PatternId (..)
, fromInstanceId
, fromInstructionId
, fromPatternId
, toInstanceId
, toInstructionId
, toPatternId
) where

import Language.InstructionSelection.Utils ( Natural
                                           , toNatural
                                           )



--------------
-- Data types
--------------

-- | Represents an instruction ID.

newtype InstructionId = InstructionId Natural
  deriving (Eq, Ord, Num, Enum)

instance Show InstructionId where
  show (InstructionId i) = show i

-- | Represents a pattern ID. Pattern IDs are used to distinguish which
-- instruction a pattern belongs to. Note, however, that an instance of a
-- pattern - which is an occurrance where a pattern has been matched over a set
-- of nodes in a function graph - is not given pattern IDs but instance IDs.

newtype PatternId = PatternId Natural
  deriving (Eq, Ord, Num, Enum)

instance Show PatternId where
  show (PatternId i) = show i

-- | Represents a pattern instance ID. Instance IDs are used to distinguish
-- between pattern an instance is based on.

newtype InstanceId = InstanceId Natural
  deriving (Eq, Ord, Num, Enum)

instance Show InstanceId where
  show (InstanceId i) = show i



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
