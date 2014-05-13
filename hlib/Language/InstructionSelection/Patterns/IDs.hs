--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.Patterns.IDs
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

module Language.InstructionSelection.Patterns.IDs (
  InstructionID (..)
, PatternID (..)
, PatternInstanceID (..)
, fromPatternInstanceID
, fromInstructionID
, fromPatternID
, toPatternInstanceID
, toInstructionID
, toPatternID
) where

import Language.InstructionSelection.Utils ( Natural
                                           , toNatural
                                           )



--------------
-- Data types
--------------

-- | Represents an instruction ID.

newtype InstructionID = InstructionID Natural
  deriving (Eq, Ord, Num, Enum)

instance Show InstructionID where
  show (InstructionID i) = show i

-- | Represents a pattern ID. Pattern IDs are used to distinguish which
-- instruction a pattern belongs to. Note, however, that an instance of a
-- pattern - which is an occurrance where a pattern has been matched over a set
-- of nodes in a function graph - is not given pattern IDs but instance IDs.

newtype PatternID = PatternID Natural
  deriving (Eq, Ord, Num, Enum)

instance Show PatternID where
  show (PatternID i) = show i

-- | Represents a pattern instance ID. Instance IDs are used to distinguish
-- between pattern an instance is based on.

newtype PatternInstanceID = PatternInstanceID Natural
  deriving (Eq, Ord, Num, Enum)

instance Show PatternInstanceID where
  show (PatternInstanceID i) = show i



-------------
-- Functions
-------------

fromPatternInstanceID :: PatternInstanceID -> Natural
fromPatternInstanceID (PatternInstanceID i) = i

toPatternInstanceID :: (Integral i) => i -> PatternInstanceID
toPatternInstanceID = PatternInstanceID . toNatural

fromInstructionID :: InstructionID -> Natural
fromInstructionID (InstructionID i) = i

toInstructionID :: (Integral i) => i -> InstructionID
toInstructionID = InstructionID . toNatural

fromPatternID :: PatternID -> Natural
fromPatternID (PatternID i) = i

toPatternID :: (Integral i) => i -> PatternID
toPatternID = PatternID . toNatural
