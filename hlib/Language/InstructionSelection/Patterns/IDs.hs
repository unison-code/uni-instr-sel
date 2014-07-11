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

module Language.InstructionSelection.Patterns.IDs
  ( PatternID (..)
  , PatternInstanceID (..)
  , fromPatternID
  , fromPatternInstanceID
  , toPatternID
  , toPatternInstanceID
  )
where

import Language.InstructionSelection.Utils
  ( Natural
  , toNatural
  )



--------------
-- Data types
--------------

newtype PatternID = PatternID Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show PatternID where
  show (PatternID i) = show i

-- | Represents a pattern instance ID. Instance IDs are used to distinguish
-- between pattern an instance is based on.

newtype PatternInstanceID = PatternInstanceID Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show PatternInstanceID where
  show (PatternInstanceID i) = show i



-------------
-- Functions
-------------

fromPatternInstanceID :: PatternInstanceID -> Natural
fromPatternInstanceID (PatternInstanceID i) = i

toPatternInstanceID :: (Integral i) => i -> PatternInstanceID
toPatternInstanceID = PatternInstanceID . toNatural

fromPatternID :: PatternID -> Natural
fromPatternID (PatternID i) = i

toPatternID :: (Integral i) => i -> PatternID
toPatternID = PatternID . toNatural
