--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Patterns.IDs
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

module Language.InstSel.Patterns.IDs
  ( PatternID (..)
  , fromPatternID
  , toPatternID
  )
where

import Language.InstSel.Utils
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



-------------
-- Functions
-------------

fromPatternID :: PatternID -> Natural
fromPatternID (PatternID i) = i

toPatternID :: (Integral i) => i -> PatternID
toPatternID = PatternID . toNatural
